#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdint.h>
 
#define IS_DIGIT(c) (c >= '0' && c <= '9')
#define BLOCKS 512
#define THREADS 512
 
bool InitCUDA()
{
    int count;
    cudaGetDeviceCount(&count);
    if (count == 0) {
        fprintf(stderr, "There is no device.\n");
        return false;
    }
 
    int i;
    for (i = 0; i < count; i++) {
        cudaDeviceProp prop;
        cudaGetDeviceProperties(&prop, i);
        if (cudaGetDeviceProperties(&prop, i) == cudaSuccess) {
            if (prop.major >= 1)  {
                break;
            }
        }
    }
 
    if (i == count) {
        fprintf(stderr, "There is no device supporting CUDA 1.x.\n");
        return false;
    }
    cudaSetDevice(i);
    return true;
}
 
size_t parseNum(std::string s, int64_t* num) {
    int64_t n = 0;
    size_t i = 0;
    while (IS_DIGIT(s[i])) {
        n *= 10;
        n += s[i] - '0';
        i++;
    }
    *num = n;
    return i;
}
 
struct range {
    int64_t dst = 0;
    int64_t src = 0;
    int64_t len = 0;
 
    range() {}
 
    range(int64_t dst, int64_t src, int64_t len) : dst(dst), src(src), len(len) {}
 
    range(std::string s) {
        size_t pos = 0;
        pos += parseNum(s.substr(pos), &dst) + 1;
        pos += parseNum(s.substr(pos), &src) + 1;
        pos += parseNum(s.substr(pos), &len);
    }
};
 
struct map {
    range *ranges;
    unsigned int len;
};
 
__global__ static void kernel(int64_t start, int64_t range, int64_t** maps, unsigned int* mapLens, int64_t* locs) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int stride = blockDim.x * gridDim.x;
 
    int64_t min = INT64_MAX;
    for (int64_t i = start + idx; i < start + range; i += stride) {
        int64_t loc = i;
        for (int m = 0; m < 7; m++) {
            for (int k = 0; k < mapLens[m]; k++) {
                int64_t dst = maps[m][k * 3];
                int64_t src = maps[m][k * 3 + 1];
                int64_t len = maps[m][k * 3 + 2];
 
                if (loc >= src && loc <= src + len) {
                    loc = dst + (loc - src);
                    break;
                }
            }
        }
 
        if (loc < min) {
            min = loc;
        }
    }
 
    if (min < INT64_MAX) {
        locs[idx] = min;
    }
}
 
void parseSeeds(std::vector<int64_t>& seeds, std::string s) {
    size_t pos = 7;
    while (pos < s.size()) {
        int64_t n;
        pos += parseNum(s.substr(pos), &n);
        seeds.push_back(n);
        pos++;
    }
}
 
void parseRanges(std::vector<std::vector<range>>& maps, std::vector<std::string>& lines) {
    size_t mapNum = 0;
    std::vector<range> m;
    std::string s;
 
    for (size_t i = 1; i < lines.size(); i++) {
        s = lines[i];
        if (IS_DIGIT(s[0])) {
            m.push_back(range(s));
        }
        else {
            maps.push_back(m);
            m = std::vector<range>();
        }
    }
 
    maps.push_back(m);
}
 
int64_t runKernel(std::vector<int64_t>& seeds, std::vector<std::vector<range>>& maps) {
    size_t numMaps = maps.size();
 
    unsigned int* mapLens;
    cudaMallocManaged(&mapLens, sizeof(unsigned int) * numMaps);
    for (size_t i = 0; i < numMaps; i++) {
        mapLens[i] = maps[i].size();
    }
 
    int64_t** maps_d;
    cudaMallocManaged(&maps_d, sizeof(int64_t*) * numMaps);
    for (size_t i = 0; i < numMaps; i++) {
        cudaMallocManaged(&maps_d[i], sizeof(int64_t) * mapLens[i] * 3);
        for (size_t j = 0; j < mapLens[i]; j++) {
            maps_d[i][j * 3] = maps[i][j].dst;
            maps_d[i][j * 3 + 1] = maps[i][j].src;
            maps_d[i][j * 3 + 2] = maps[i][j].len;
        }
    }
 
    int64_t totMin = INT64_MAX;
    for (size_t i = 0; i < seeds.size(); i += 2) {
        int64_t start = seeds.at(i);
        int64_t end = seeds.at(i + 1);
 
        int64_t* locs;
        cudaMallocManaged(&locs, sizeof(int64_t) * BLOCKS * THREADS);
        for (size_t i = 0; i <  BLOCKS * THREADS; i++) {
            locs[i] = INT64_MAX;
        }
 
        kernel<<<BLOCKS, THREADS>>>(start, end, maps_d, mapLens, locs);
        cudaDeviceSynchronize();
 
        int64_t min = INT64_MAX;
        for (size_t j = 0; j < BLOCKS * THREADS; j++) {
            if (locs[j] < min) {
                min = locs[j];
            }
        }
 
        if (min < totMin) {
            totMin = min;
        }
 
        cudaFree(locs);
    }
 
    for (size_t i = 0; i < numMaps; i++) {
        cudaFree(maps_d[i]);
    }
 
    cudaFree(maps_d);
    cudaFree(mapLens);
 
    return totMin;
}
 
int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "No input file" << std::endl;
        return -1;
    }
 
    if (!InitCUDA()) {
        return -1;
    }
 
    std::ifstream input(argv[1]);
    std::string s;
    std::vector<std::string> lines;
    std::vector<int64_t> seeds;
    std::vector<std::vector<range>> maps;
 
    getline(input, s);
    parseSeeds(seeds, s);
 
    while (getline(input, s)) {
        if (!s.empty())
            lines.push_back(s);
    }
 
    input.close();
 
    parseRanges(maps, lines);
 
    int64_t part2 = runKernel(seeds, maps);
 
    std::cout << "Part 2: " << part2 << std::endl;
    return 0;
}