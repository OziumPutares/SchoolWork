#include <algorithm>
#include <array>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <filesystem>
#include <format>
#include <fstream>
#include <iostream>
#include <iterator>
#include <print>
#include <random>
#include <streambuf>
#include <string>

template <std::size_t numberOfPlayers, typename ResultType, typename DistType,
          typename Engine>
//  requires requires(Engine &e, DistType &dist, std::random_device rd) {
//    e.seed(rd());
//    dist(e)->ResultType;
//  }
[[nodiscard]]
constexpr auto generate(DistType &dist, Engine &generator) -> decltype(auto) {
  std::array<ResultType, numberOfPlayers> results;
  std::random_device rd{};
  std::ranges::generate(results, [&]() mutable {
    generator.seed(rd());
    return (dist(generator));
  });
  return results;
}
template <std::size_t numberOfPlayers, std::size_t numberOfRounds,
          typename DistType, typename Engine>
[[nodiscard]]
constexpr std::array<std::array<std::int64_t, numberOfPlayers>, numberOfRounds>
generateMultipleRounds(DistType &dist, Engine &generator) {
  std::array<std::array<std::int64_t, numberOfPlayers>, numberOfRounds> results;
  std::ranges::generate(results, [&dist, &generator]() {
    return generate<numberOfPlayers, std::int64_t>(dist, generator);
  });
  return results;
}
template <std::size_t numberOfPlayers, std::size_t numberOfRounds>
[[nodiscard]]
constexpr std::array<std::size_t, numberOfPlayers> scorePlayers(
    std::array<std::array<std::int64_t, numberOfPlayers>, numberOfRounds> rolls,
    std::array<std::size_t, numberOfPlayers> startingScores = {}) {
  std::ranges::for_each(rolls, [&startingScores](auto round) {
    auto max = std::ranges::max_element(round);
    if (max == std::ranges::end(round))
      return;
    if (std::ranges::count(round, *max) > 1)
      return;
    startingScores[std::ranges::distance(std::begin(round), max)]++;
  });
  return startingScores;
}

template <std::size_t numberOfPlayers>
[[nodiscard]]
constexpr std::array<std::size_t, numberOfPlayers>
readPlayerScores(std::filesystem::path pathOfFile) {
  auto startingScores = std::array<std::size_t, 3>{};
  if (std::filesystem::is_regular_file(pathOfFile)) {
    {
      auto inputFile = std::ifstream("./lol.txt");
      auto line = std::string{};
      auto currentPlayer = 0;
      while (std::getline(inputFile, line)) {
        try {
          startingScores[currentPlayer] = std::stoull(line);
          currentPlayer++;
        } catch (std::exception &err) {
          std::cerr << err.what();
          throw err;
        }
      }
    }
  }
  return startingScores;
}

int main() {
  auto generator = std::default_random_engine{};
  auto dist = std::uniform_int_distribution<std::int64_t>(1, 6);
  auto outputToFile = std::string{};
  auto startingScores =
      readPlayerScores<3>(std::filesystem::current_path() / "lol.txt");
  for (auto lslfksj : startingScores) {
    std::print("{},", lslfksj);
  }
  std::ranges::for_each(
      scorePlayers(generateMultipleRounds<3, 5>(dist, generator),
                   startingScores),
      [&outputToFile](auto i) { outputToFile += std::format("{}\n", i); });
  std::ofstream("./lol.txt") << outputToFile;
}
