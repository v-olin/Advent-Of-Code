#include <vector>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>

#define W 140
#define H 140

#define IS_SYMBOL(x,y,ps) (x >= 0 && x < W && y >= 0 && y < H && ps[x][y] != '.')

struct coord {
    size_t x, y;

    bool operator==(const coord& other) const {
        return x == other.x && y == other.y;
    }
};

struct coordhash {
    size_t operator()(const coord& c) const {
        return c.x * 1000 + c.y;
    }
};

enum direction {
    UP,
    DOWN,
    LEFT,
    RIGHT
};

struct player {
    coord pos;
    direction dir;
};

using pipes = std::vector<std::string>;

player findStart(const pipes& pipes) {
    coord start = { 0,0 };
    for (size_t x = 0; x < pipes.size(); x++) {
        for (size_t y = 0; y < pipes[x].length(); y++) {
            if (pipes[x][y] == 'S')
                start = { x, y };
        }
    }

    if (IS_SYMBOL(start.x, start.y - 1, pipes))
        return { {start.x, start.y - 1}, LEFT };
    if (IS_SYMBOL(start.x, start.y + 1, pipes))
        return { {start.x, start.y + 1}, RIGHT };
    if (IS_SYMBOL(start.x - 1, start.y, pipes))
        return { {start.x - 1, start.y}, UP };

    return { {start.x + 1, start.y}, DOWN };
}

void walk(player& p, const pipes& pipes) {
    switch (pipes[p.pos.x][p.pos.y]) {
    case '|':
        if (p.dir == UP)
            p.pos.x--;
        else if (p.dir == DOWN)
            p.pos.x++;
        break;
    case '-':
        if (p.dir == LEFT)
            p.pos.y--;
        else if (p.dir == RIGHT)
            p.pos.y++;
        break;
    case 'L':
        if (p.dir == DOWN) {
            p.pos.y++;
            p.dir = RIGHT;
        }
        else if (p.dir == LEFT) {
            p.pos.x--;
            p.dir = UP;
        }
        break;
    case 'J':
        if (p.dir == DOWN) {
            p.pos.y--;
            p.dir = LEFT;
        }
        else if (p.dir == RIGHT) {
            p.pos.x--;
            p.dir = UP;
        }
        break;
    case '7':
        if (p.dir == UP) {
            p.pos.y--;
            p.dir = LEFT;
        }
        else if (p.dir == RIGHT) {
            p.pos.x++;
            p.dir = DOWN;
        }
        break;
    case 'F':
        if (p.dir == UP) {
            p.pos.y++;
            p.dir = RIGHT;
        }
        else if (p.dir == LEFT) {
            p.pos.x++;
            p.dir = DOWN;
        }
        break;
    default:
        // std::cerr << "Unknown pipe: " << pipes[p.pos.x][p.pos.y] << std::endl;
        break;
    }
}

size_t part1(const pipes& pipes) {
    player p = findStart(pipes);
    size_t steps = 1;

    while (pipes[p.pos.x][p.pos.y] != 'S') {
        walk(p, pipes);
        steps++;
    }

    return steps >> 1;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        std::cout << "Usage: " << argv[0] << " <input file>" << std::endl;
        return 1;
    }

    std::ifstream input(argv[1]);
    pipes pipes;
    std::string line;
    size_t row = 0;
    while (std::getline(input, line)) {
        pipes.push_back(line);
    }

    std::cout << "Part 1: " << part1(pipes) << std::endl;

    return 0;
}