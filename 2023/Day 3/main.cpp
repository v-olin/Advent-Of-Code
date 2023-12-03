#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <unordered_set>
#include <regex>

#define INPUT "input.txt"
#define IS_DIGIT(c) (c >= '0' && c <= '9')

using namespace std;

struct coord {
    int x;
    int y;

    bool operator==(const coord& other) const noexcept {
        return x == other.x && y == other.y;
    };

    size_t operator()(const coord& toHash) const noexcept {
        size_t hash = toHash.x * 200 + toHash.y;
        return hash;
    };
};

struct symbol {
    char val;
    coord c;

    bool operator==(const symbol& other) const noexcept {
        return c.x == other.c.x && c.y == other.c.y && val == other.val;
    };

    bool operator==(const coord& other) const noexcept {
        return c.x == other.x && c.y == other.y;
    };

    size_t operator()(const symbol& toHash) const noexcept {
        size_t hash = toHash.c.x * 200 + toHash.c.y;
        return hash;
    };
};

struct num {
    int val = INT_MAX;
    coord c;
    size_t width;
};

namespace std {
    template <>
    struct hash<symbol> {
        size_t operator()(const symbol& s) const {
            return s(s);
        }
    };

    template <>
    struct hash<coord> {
        size_t operator()(const coord& c) const {
            return c(c);
        }
    };
}

struct schema {
    vector<string> lines;
    unordered_set<symbol, std::hash<symbol>> symbols;
    vector<num> numbers;
};

void parseSchema(schema& schema) {
    regex r("(([0-9]+)|([^.]))");
    smatch m;
    int i = 1;

    for (auto& line : schema.lines) {
        if (regex_search(line, m, r)) {
            sregex_iterator it(line.begin(), line.end(), r);
            for (; it != sregex_iterator(); ++it) {
                m = *it;
                if (IS_DIGIT(m.str()[0])) {
                    num n = {stoi(m.str()), {i, m.position() + 1}, m.length()};
                    schema.numbers.push_back(n);
                } else {
                    symbol s = {m.str()[0], {i, m.position() + 1}};
                    schema.symbols.insert(s);
                }
            }
        }
        i++;
    }
}

int part1(schema& schema) {
    int sum = 0;
    for (num& n : schema.numbers) {
        bool engPart = false;

        for (int x = 0; x <= 2; x++) {
            for (size_t y = 0; y < n.width + 2; y++) {
                coord c = {n.c.x + x - 1, n.c.y + static_cast<int>(y) - 1};
                for (auto& sym : schema.symbols) {
                    if (sym.c == c) {
                        engPart = true;
                        break;
                    }
                }
            }
        }
        sum += engPart ? n.val : 0;
    }

    return sum;
}

int part2(schema& schema) {
    int sum = 0;

    for (const symbol& sym : schema.symbols) {
        if (sym.val != '*')
            continue;
        
        num a, b;
        for (num& n : schema.numbers) {
            if (sym.c.x >= n.c.x - 1 && sym.c.x <= n.c.x + 1) {
                if (sym.c.y >= n.c.y - 1 && sym.c.y <= n.c.y + n.width) {
                    if (a.val == INT_MAX)
                        a = n;
                    else
                        b = n;
                    if (a.val != INT_MAX && b.val != INT_MAX) {
                        sum += a.val * b.val;
                        break;
                    }
                }
            }
        }
    }

    return sum;
}

int main() {
    ifstream in(INPUT);
    vector<string> lines;
    schema schema;
    string s;

    while (getline(in, s)) {
        schema.lines.push_back(s);
    }

    parseSchema(schema);

    cout << "Part 1: " << part1(schema) << endl;
    cout << "Part 2: " << part2(schema) << endl;

    return 0;
}