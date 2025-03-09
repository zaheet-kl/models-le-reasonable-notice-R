prophetSource('predictors', 'util', 'util.R')

describe('convert_probability_above_50', {
  it('converts a probability below .5 to the inverse probability', {
    expect_equal(convert_probability_above_50(0.2), 0.8)
  })

  it('does not change a probability above .5', {
    expect_equal(convert_probability_above_50(0.8), 0.8)
  })
})
