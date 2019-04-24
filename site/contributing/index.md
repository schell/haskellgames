---
title: Contributing
articleTitle: Contributing a New Game &#x1F579;&#xFE0F;
theme: templates/default.html
toc-title: Contributing
---

Writing a new game
------------------
There are very few steps to contribute an article (and not all of these must be
followed):

  * decide which game you'd like to implement
  * if you like you can then [edit this page and make a PR][contribute_pr] to
    reserve your game so others don't start working on it
  * write your game as a [literate haskell file](https://wiki.haskell.org/Literate_programming)
  * when you're ready to publish please [edit the sitemap.yaml file and make a PR][sitemap_pr]
    to create a new page in the games section from your lhs file. The `sitemap.yaml`
    file has at least one example in it already.
  * if you would like to see your changes before publishing it to http://haskellgames.com
    you can set your PR to merge into the `staging` branch. Merging a PR into
    staging will trigger a build and deploy to http://staging.haskellgames.com
    where you can proof your work. `staging` will then be merged into `master`
    and your article will be published.

[contribute_pr]: https://github.com/schell/haskellgames/edit/staging/site/contributing/index.md
[sitemap_pr]: https://github.com/schell/haskellgames/edit/staging/sitemap.yaml

Games in progress
-----------------
Below is a list of the games that are currently being worked on, or games that
would be a lot of fun to implement. Choose one! Add some!


| game         | who              | status      | topics        |
|--------------|------------------|-------------|---------------|
| pong         | [schell][schell] | in progress | frp           |
| snake        | [schell][schell] | done        | mtl, nonempty |
| billiards    | [kynan][kynan]   | reserved    | physics       |
| asteroids    |                  |             |               |
| pac-man      |                  |             |               |
| pole position|                  |             |               |
| rogue        |                  |             |               |
| spaceinvaders|                  |             |               |
| zaxxon       |                  |             |               |
| zelda        |                  |             |               |

[kynan]: https://github.com/ublubu
[schell]: https://github.com/schell
