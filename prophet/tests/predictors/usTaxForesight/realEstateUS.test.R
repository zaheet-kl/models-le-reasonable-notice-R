prophetSource('predictors', 'usTaxForesight', 'realEstateUS.R')

# we only need to create a data frame with the variables that are being transformed and test those
# numerics are passsed from blue-j-api to prophet as strings
df <- data.frame('X4.1'=factor('1'),
                 'X7.1'=factor('1'),
                 'X7.2'=factor('1'),
                 'X7.3'=factor('1'),
                 'X8.1'=factor('1'))

if(is.na(df$X7.1)){
  df$X7.1log <- 0
  df$X7.2log <- 0
  df$X7.3log <- 0
  df$X8.1log <- 0
} else  {
  df$X7.1log <- log(1 + as.numeric(as.character(df$X7.1)))
  df$X7.2log <- log(1 + as.numeric(as.character(df$X7.2)))
  df$X7.3log <- log(1 + as.numeric(as.character(df$X7.3)))
  df$X8.1log <- log(1 + as.numeric(as.character(df$X8.1)))
}

describe('real_estate_us_transform_data', {
  it('converts type of 4.1 to numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X4.1), TRUE)
  })
  
  it('converts type of 7.1 to numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.1), TRUE)
  })
  
  it('converts type of 7.2 to numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.2), TRUE)
  })
  
  it('converts type of 7.3 to numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.3), TRUE)
  })
  
  it('converts type of 8.1 to numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X8.1), TRUE)
  })
  
  it('confirms 7.1log is numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.1log), TRUE)
  })
  
  it('confirms 7.2log is numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.2log), TRUE)
  })
  
  it('confirms 7.3log is numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X7.3log), TRUE)
  })
  
  it('confirms 8.1log is numeric', {
    expect_equal(is.numeric(real_estate_us_transform_data(df)$X8.1log), TRUE)
  })
  
  it('confirms value of 7.1log', {
    expect_equal(real_estate_us_transform_data(df)$X7.1log, log(1 + as.numeric(as.character(df$X7.1))))
  })
  
  it('confirms value of 7.2log', {
    expect_equal(real_estate_us_transform_data(df)$X7.2log, log(1 + as.numeric(as.character(df$X7.2))))
  })
  
  it('confirms value of 7.3log', {
    expect_equal(real_estate_us_transform_data(df)$X7.3log, log(1 + as.numeric(as.character(df$X7.3))))
  })
  
  it('confirms value of 8.1log', {
    expect_equal(real_estate_us_transform_data(df)$X8.1log, log(1 + as.numeric(as.character(df$X8.1))))
  })
  
})
