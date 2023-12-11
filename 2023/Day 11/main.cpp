#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <utility>
#include <unordered_map>
#include <cassert>

using namespace std;

#define abs(x) ((x) < 0 ? -(x) : (x))
#define max(x,y) ((x) > (y) ? (x) : (y))
#define min(x,y) ((x) < (y) ? (x) : (y))
#define EMPTY_SIZE 1000000

struct coord {
    size_t x, y;

    bool operator==(const coord& other) const {
        return (x == other.x && y == other.y)
                || (x == other.y && y == other.x);
    }
};

struct emptyMap {
    vector<bool> cols;
    vector<bool> rows;

    emptyMap(size_t x, size_t y) {
        cols = vector<bool>(x, true);
        rows = vector<bool>(y, true);
    }
};

emptyMap findEmpties(vector<string> map) {
    emptyMap empties(map[0].size(), map.size());
    for (size_t y = 0; y < map.size(); y++) {
        for (size_t x = 0; x < map[y].size(); x++) {
            empties.cols[x] = empties.cols[x] & map[y][x] == '.';
            empties.rows[y] = empties.rows[y] & map[y][x] == '.';
        }
    }
    return empties;
}

vector<string> expand(const vector<string> map, const emptyMap empties) {
    vector<string> newMap;
    size_t width = map[0].length() + count(empties.cols.begin(), empties.cols.end(), true);
    for (size_t y = 0; y < map.size(); y++) {
        string row;
        if (empties.rows[y]) {
            row = string(width, '.');
            newMap.push_back(row);
        } else {
            for (size_t x = 0; x < map[y].length(); x++) {
                row += map[y][x];
                if (empties.cols[x]) {
                    row += '.';
                }
            }
        }
        newMap.push_back(row);
    }
    return newMap;
}

unordered_map<size_t, coord> findGalaxies(vector<string> map) {
    unordered_map<size_t, coord> galaxies;
    size_t galaxy = 1;
    for (size_t y = 0; y < map.size(); y++) {
        for (size_t x = 0; x < map[y].size(); x++) {
            if (map[y][x] == '#') {
                galaxies[galaxy++] = {x, y};
            }
        }
    }
    return galaxies;
}

vector<pair<size_t, size_t>> getPairs(unordered_map<size_t, coord> galaxies) {
    vector<pair<size_t, size_t>> pairs;
    for (size_t i = 1; i <= galaxies.size(); i++) {
        for (size_t j = i + 1; j <= galaxies.size(); j++) {
            pairs.push_back({i, j});
        }
    }
    return pairs;
}

inline
size_t distance(coord a, coord b) {
    return abs(max(a.x, b.x) - min(a.x, b.x))
         + abs(max(a.y, b.y) - min(a.y, b.y));
}

size_t part1(vector<string> map) {
    unordered_map<size_t, coord> galaxies = findGalaxies(map);
    vector<pair<size_t, size_t>> pairs = getPairs(galaxies);

    size_t totDistance = 0;
    for (auto pair : pairs) {
        // size_t tmp = distance(galaxies[pair.first], galaxies[pair.second]);
        totDistance += distance(galaxies[pair.first], galaxies[pair.second]);
    }
    return totDistance;
}

pair<size_t, size_t> emptyCrossings(const coord from, const coord to, const emptyMap empties) {
    size_t rows = 0, cols = 0;    
    for (size_t y = min(from.y, to.y) + 1; y < max(from.y, to.y); y++) {
        if (empties.rows[y])
            rows++;
    }
    for (size_t x = min(from.x, to.x) + 1; x < max(from.x, to.x); x++) {
        if (empties.cols[x])
            cols++;
    }
    return make_pair(rows, cols);
}

size_t distance2(coord a, coord b, const emptyMap empties) {
    size_t dist = distance(a, b);
    auto tmp = emptyCrossings(a, b, empties);
    dist += tmp.first * EMPTY_SIZE + tmp.second * EMPTY_SIZE;
    dist -= tmp.first + tmp.second;
    return dist;
}

size_t part2(vector<string> map) {
    unordered_map<size_t, coord> galaxies = findGalaxies(map);
    vector<pair<size_t, size_t>> pairs = getPairs(galaxies);
    emptyMap empties = findEmpties(map);

    size_t totDistance = 0;
    for (auto pair : pairs) {
        size_t tmp = distance2(galaxies[pair.first], galaxies[pair.second], empties);
        totDistance += tmp;
    }
    return totDistance;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        cerr << "No input file" << endl;
        return -1;
    }

    ifstream input(argv[1]);
    vector<string> lines;
    string line;
    while (getline(input, line)) {
        lines.push_back(line);
    }

    emptyMap empties = findEmpties(lines);
    vector<string> newMap = expand(lines, empties);

    auto p1 = part1(newMap);
    cout << "Part 1: " << p1 << endl;
    assert(p1 == 10422930 && "Part 1 failed");
    
    auto p2 = part2(lines);
    cout << "Part 2: " << p2 << endl;
    assert(p2 == 699909023130 && "Part 2 failed");

    return 0;
}