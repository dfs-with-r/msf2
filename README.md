 [![Travis build status](https://travis-ci.org/dfs-with-r/msf2.svg?branch=master)](https://travis-ci.org/dfs-with-r/msf2)

## msf2
msf2 is an R wrapper for the [MySportsFeeds API](https://www.mysportsfeeds.com/).

There is an [official R package](https://github.com/MySportsFeeds/mysportsfeeds-r) provided by MySportsFeeds, but this package differs in the following ways:

- Provides `tidy()` function to convert a JSON list into a tidy dataframe. 
- Provides `delay` parameter in API calls to execute multiple queries with a delay between them in order to obey rate limits.
- Provides common defaults for some parameters (ex. uses `season = "current"`) to reduce typing.

These additions have been very helpful for me to use the data effectively in R.

## Install
```R
devtools::install_github("dfs-with-r/msf2")
```

## Usage
To start, make sure you have set the following environment variable set on your computer. You need to sign up for an account at [MySportsFeeds](https://www.mysportsfeeds.com) then create an API key. Once you have this key, use the `add_key()` in this package to help show you what to do with it.

```R
add_key("xxxyyyzzz")
```

Once you have done this you can query data easily. See the function documentation for a description of the parameters. They should follow the same parameters required to query the official web API.

```R
library(msf)

# Get data
json <- game_boxscore("nba", "20171027-BRO-NYK", season = "2017-2018-regular")

# Or parse the json list into tidy dataframe
boxscore <- tidy(json)
```
