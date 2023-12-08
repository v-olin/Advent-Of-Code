#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <stdint.h>
#include <numeric>

enum type {
    START,
    GOAL,
    NONE
};

struct node {
    type t;  
    int l;
    int r;
};

struct map {
    std::string dirs;
    std::unordered_map<int, node> nodes;
};

int toId(std::string s) {
    int id = 0;
    for (char c : s) {
        id = id * 26 + (c - 'A');
    }
    return id;
}

void parse(map& m, std::string line) {
    int id = toId(line.substr(0, 3));
    int l = toId(line.substr(7, 3));
    int r = toId(line.substr(12, 3));
    type t = NONE;
    if (line[2] == 'A') t = START;
    else if (line[2] == 'Z') t = GOAL;

    m.nodes[id] = {t, l, r};
}

int part1(map m) {
    int curr = toId("AAA"), goal = toId("ZZZ");
    int steps = 0, instr = 0;


    while (curr != goal) {
        char c = m.dirs[instr++ % m.dirs.size()];
        curr = (c == 'L') ? m.nodes[curr].l : m.nodes[curr].r;
        steps++;
    }

    return steps;
}

int simulate(map m, int start) {
    int curr = start;
    int steps = 0, instr = 0;

    while (m.nodes[curr].t != GOAL) {
        char c = m.dirs[instr++ % m.dirs.size()];
        curr = (c == 'L') ? m.nodes[curr].l : m.nodes[curr].r;
        steps++;
    }

    return steps;
}

uint64_t part2(map m) {
    uint64_t lm = 1;
    for (auto it = m.nodes.begin(); it != m.nodes.end(); it++) {
        if (it->second.t == START) {
            int steps = simulate(m, it->first);
            lm = std::lcm(lm, steps);
        }
    }

    return lm;
}

int main(int argc, char** argv) {

    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <input file>" << std::endl;
        return 1;
    }

    std::ifstream input(argv[1]);
    std::string line;
    std::vector<std::string> lines;
    std::getline(input, line);
    map m;
    m.dirs = line;
    std::getline(input, line);

    while (std::getline(input, line)) {
        parse(m, line);
    }

    std::cout << "Part 1: " << part1(m) << std::endl;
    std::cout << "Part 2: " << part2(m) << std::endl;

    return 0;
}