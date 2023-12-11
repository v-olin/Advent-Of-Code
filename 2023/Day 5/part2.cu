#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <stdint.h>

#define IS_DIGIT(c) (c >= '0' && c <= '9')
#define ULL unsigned long long
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

size_t parseNum(std::string s, uint64_t* num) {
    uint64_t n = 0;
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
    uint64_t dst = 0;
    uint64_t src = 0;
    uint64_t len = 0;

    range() {}

    range(uint64_t dst, uint64_t src, uint64_t len) : dst(dst), src(src), len(len) {}

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

__global__ static void kernel(ULL start, ULL end, ULL** maps, unsigned int* mapLens, ULL* locs) {
    int idx = blockIdx.x * blockDim.x + threadIdx.x;
    int stride = blockDim.x * gridDim.x;

    ULL min = UINT64_MAX;
    for (ULL i = start + idx; i <= end; i += stride) {
        ULL loc = i;
        for (int m = 0; m < 7; m++) {
            for (int k = 0; k < mapLens[m]; k++) {
                ULL dst = maps[m][k * 3];
                ULL src = maps[m][k * 3 + 1];
                ULL len = maps[m][k * 3 + 2];

                if (loc >= src && loc < src + len) {
                    loc = dst + loc - src;
                    break;
                }
            }
        }

        if (loc < min) {
            min = loc;
        }
    }

    if (min < UINT64_MAX) {
        locs[idx] = min;
    }
}

void parseSeeds(std::vector<uint64_t>& seeds, std::string s) {
    size_t pos = 7;
    while (pos < s.size()) {
        uint64_t n;
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

ULL runKernel(std::vector<uint64_t>& seeds, std::vector<std::vector<range>>& maps) {
    size_t numMaps = maps.size();

    unsigned int* mapLens;
    cudaMallocManaged(&mapLens, sizeof(unsigned int) * numMaps);
    for (size_t i = 0; i < numMaps; i++) {
        mapLens[i] = maps[i].size();
    }

    ULL** maps_d;
    cudaMallocManaged(&maps_d, sizeof(ULL*) * numMaps);
    for (size_t i = 0; i < numMaps; i++) {
        cudaMallocManaged(&maps_d[i], sizeof(ULL) * mapLens[i] * 3);
        for (size_t j = 0; j < mapLens[i]; j++) {
            maps_d[i][j * 3] = maps[i][j].dst;
            maps_d[i][j * 3 + 1] = maps[i][j].src;
            maps_d[i][j * 3 + 2] = maps[i][j].len;
        }
    }

    ULL totMin = UINT64_MAX;
    for (size_t i = 0; i < seeds.size(); i += 2) {
        ULL start = seeds.at(i);
        ULL end = seeds.at(i + 1);

        ULL* locs;
        cudaMallocManaged(&locs, sizeof(ULL) * BLOCKS * THREADS);

        kernel<<<BLOCKS, THREADS>>>(start, end, maps_d, mapLens, locs);
        cudaDeviceSynchronize();

        ULL min = UINT64_MAX;
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
    std::vector<uint64_t> seeds;
    std::vector<std::vector<range>> maps;

    getline(input, s);
    parseSeeds(seeds, s);

    while (getline(input, s)) {
        if (!s.empty())
            lines.push_back(s);
    }
    
    input.close();

    parseRanges(maps, lines);

    std::cout << "Seeds: " << seeds.size() << std::endl;
    std::cout << "Maps: " << maps.size() << std::endl;

    for (size_t i = 0; i < maps.size(); i++) {
        std::cout << "Map " << i << ": " << maps[i].size() << std::endl;
    }

    ULL part2 = runKernel(seeds, maps);
    
    std::cout << "Part 2: " << part2 << std::endl;
    return 0;
}