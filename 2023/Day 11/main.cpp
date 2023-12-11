#include <fstream>
#include <string>
#include <vector>
#include <iostream>
#include <algorithm>
#include <utility>
#include <unordered_map>
#include <cassert>

using namespace std;

#define max(x,y) ((x) > (y) ? (x) : (y))
#define min(x,y) ((x) < (y) ? (x) : (y))

struct coord { size_t x, y; };

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

pair<size_t, size_t> distance(coord a, coord b, const emptyMap empties) {
    size_t manhattan = max(a.x, b.x) - min(a.x, b.x) + max(a.y, b.y) - min(a.y, b.y);
    auto crossings = emptyCrossings(a, b, empties);
    size_t n = crossings.first + crossings.second;
    return make_pair(manhattan + n, manhattan + n * 999999);
}

pair<size_t, size_t> solve(vector<string> map) {
    unordered_map<size_t, coord> galaxies = findGalaxies(map);
    vector<pair<size_t, size_t>> pairs = getPairs(galaxies);
    emptyMap empties = findEmpties(map);

    pair<size_t, size_t> totDistance = make_pair(0, 0);
    for (auto pair : pairs) {
        auto distances = distance(galaxies[pair.first], galaxies[pair.second], empties);
        totDistance.first += distances.first;
        totDistance.second += distances.second;
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

    pair<size_t, size_t> solution = solve(lines);
    cout << "Part 1: " << solution.first << endl;
    cout << "Part 2: " << solution.second << endl;

    return 0;
}