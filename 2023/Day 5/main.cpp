#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <regex>
#include <stdint.h>
#include <omp.h>
#include <queue>

#define IS_DIGIT(c) (c >= '0' && c <= '9')

using namespace std;

size_t parseNum(string s, uint64_t* num) {
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

    range(string s) {
        size_t pos = 0;
        pos += parseNum(s.substr(pos), &dst) + 1;
        pos += parseNum(s.substr(pos), &src) + 1;
        pos += parseNum(s.substr(pos), &len);
    }
};

void parseSeeds(vector<uint64_t>& seeds, string s) {
    size_t pos = 7;
    cout << "parsing: \"" << s.substr(pos) << "\"" << endl;

    while (pos < s.size()) {
        uint64_t n;
        pos += parseNum(s.substr(pos), &n);
        seeds.push_back(n);
        pos++;
    }
}

void parseRanges(vector<vector<range>>& maps, vector<string>& lines) {
    size_t mapNum = 0;
    vector<range> m;
    string s;

    for (size_t i = 1; i < lines.size(); i++) {
        s = lines[i];
        if (IS_DIGIT(s[0])) {
            m.push_back(range(s));
        }
        else {
            maps.push_back(m);
            m = vector<range>();
        }
    }

    maps.push_back(m);
}

uint64_t translate(uint64_t seed, vector<vector<range>>& maps) {
    uint64_t n = seed;

    for (vector<range>& m : maps) {
        for (range& r : m) {
            if (n >= r.src && n <= r.src + r.len) {
                n = r.dst + (n - r.src);
                break;
            }
        }
    }

    return n;
}

uint64_t part1(vector<uint64_t>& seeds, vector<vector<range>>& maps) {
    uint64_t min = UINT64_MAX;

    for (uint64_t seed : seeds) {
        uint64_t n = translate(seed, maps);
        if (n < min)
            min = n;
    }

    return min;
}

uint64_t part2(vector<uint64_t>& seeds, vector<vector<range>>& maps) {
    queue<range> toTranslate, tmp;

    for (auto& r : maps[0]) {
        toTranslate.push(r);
    }

    for (size_t i = 1; i < maps.size(); i++) {
        while (!toTranslate.empty()) {
            range r = toTranslate.front();
            toTranslate.pop();
            for (auto& next : maps[i]) {
                if (r.dst >= next.src && r.dst <= next.src + next.len) {
                    if (r.dst + (r.src - next.src) < next.dst + next.len) {
                        tmp.push(range(r.dst + (r.src - next.src), next.dst, next.len));
                    }
                    else {
                        tmp.push(range(next.dst + next.len, r.dst + (r.src - next.src) - (next.dst + next.len), r.len - (r.dst + (r.src - next.src) - (next.dst + next.len))));
                    }
                }
            }
        }
        toTranslate.swap(tmp);
    }

    return 0;
}

int main(int argc, char** argv) {

    if (argc != 2) {
       cout << "No input file" << endl;
       return 1;
    }

    ifstream input(argv[1]);
    string s;
    vector<string> lines;
    vector<uint64_t> seeds;
    vector<vector<range>> maps;

    getline(input, s);
    parseSeeds(seeds, s);
    cout << "seeds: " << seeds.size() << endl;

    while (getline(input, s)) {
        if (!s.empty())
            lines.push_back(s);
    }
    
    input.close();

    parseRanges(maps, lines);
    int numRanges = 0;

    for (auto& m : maps) {
        numRanges += m.size();
    }

    cout << "ranges: " << numRanges << endl; // 1000

    cout << "Part 1: " << part1(seeds, maps) << endl;

    return 0;
}