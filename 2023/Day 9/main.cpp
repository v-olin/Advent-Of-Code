#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <stdint.h>
#include <algorithm>

std::vector<long long> toHistory(const std::string& line) {
    std::vector<long long> h;
    size_t pos = 0;
    while (pos < line.size()) {
        size_t next = line.find(' ', pos);
        if (next == std::string::npos) {
            next = line.size();
        }
        h.push_back(std::stoll(line.substr(pos, next-pos)));
        pos = next+1;
    }
    return h;
}

long long speculate(std::vector<long long> h) {
    size_t n = h.size();
    std::vector<long long> diff(n-1);
    bool allSame = true;
    for (size_t i = 0; i < n-1; i++) {
        diff[i] = h[i+1] - h[i];
        if (diff[i] != diff[0]) {
            allSame = false;
        }
    }

    if (allSame) {
        return h[n-1] + diff[0];
    }

    return h[n-1] + speculate(diff);
}

long long part1(const std::vector<std::vector<long long>>& lines) {
    long long sum = 0;
    for (const std::vector<long long>& h : lines) {
        long long n = speculate(h);
        sum += n;
    }
    return sum;
}

long long part2(std::vector<std::vector<long long>>& lines) {
    long long sum = 0;
    for (std::vector<long long>& h : lines) {
        std::reverse(h.begin(), h.end());
        long long n = speculate(h);
        sum += n;
    }
    return sum;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <input file>" << std::endl;
        return 1;
    }

    std::ifstream input(argv[1]);
    std::string line;
    std::vector<std::vector<long long>> lines;
    while (std::getline(input, line)) {
        lines.push_back(toHistory(line));
    }

    std::cout << "Part 1: " << part1(lines) << std::endl;
    std::cout << "Part 2: " << part2(lines) << std::endl;

    return 0;
}