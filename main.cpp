#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <print>
#include <random>

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

int main() {
  auto generator = std::default_random_engine{};
  auto dist = std::uniform_int_distribution<std::int64_t>(1, 6);
  auto points = std::array<std::size_t, 3>{};
  for (auto it : generateMultipleRounds<3, 5>(dist, generator)) {
    auto maxScore = 0;
    auto maxPlayerNumberId = -1;
    auto currentId = 0;
    for (auto score : it) {
      if (score > maxScore) {
        maxPlayerNumberId = currentId;
        maxScore = score;
      } else if (score == maxScore) {
        std::print("Current score: {}, maxScore: {} \t", score, maxScore);
        maxPlayerNumberId = -1;
      }
      currentId++;

      std::print("{},", score);
    }
    std::println();
    if (maxPlayerNumberId == -1) {
      std::println("No max player score");
      continue;
    }
    points[maxPlayerNumberId]++;
  }
  for (auto playerScore : points)
    std::println("Score: {}", playerScore);
}
