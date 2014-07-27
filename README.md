chess
=====

An other UCI engine written in Haskell


This program is still in an early stage, but we managed to beat pyChess at its strongest setting.

The game PGN:

~~~
[Event "Local Event"]
[Site "Local Site"]
[Date "2014.07.27"]
[Round "1"]
[White "Chess"]
[Black "PyChess 0.10.1"]
[Result "1-0"]
[TimeControl "900+5"]
[Time "10:16:00"]
[WhiteClock "0:00:19.138"]
[BlackClock "0:12:44.121"]
[PlyCount "43"]

1. e4 c5 2. Nf3 d6 3. Nc3 Nf6 4. Bb5+ Bd7 5. Bxd7+ Nbxd7 6. O-O h6 7. d4 cxd4
8. Nxd4 e5 9. Nf5 Nc5 10. Re1 h5 11. Bg5 Qb8 12. Bxf6 gxf6 13. Nb5 Qd8 14. b4
Nxe4 15. Rxe4 d5 16. Qxd5 Qxd5 17. Nc7+ Kd7 18. Nxd5 Rb8 19. Rd1 Ke6 20. Nde7
Rd8 21. Rxd8 b6 22. Rd6# 1-0
~~~


Haddock
=======

The auto generated haskell code documentation lives on http://phaul.github.io/chess/. This is manually regenerated from time to time, and might not reflect the latest code base.


Testing
=======

The [STS](https://sites.google.com/site/strategictestsuite) run with depth set to 6. Total points for STS1-10: 336, STS1-13: 410, STS1-14: 452.

| STS                                                  | Points | Test                                         |
|:-----------------------------------------------------|-------:|:---------------------------------------------|
| [STS1](http://phaul.github.io/chess/STS1.epd.html)   | 24     | Undermine									   |
| [STS2](http://phaul.github.io/chess/STS2.epd.html)   | 41     | Open Files and Diagonals					   |
| [STS3](http://phaul.github.io/chess/STS3.epd.html)   | 26     | Knight Outposts/Repositioning/Centralization |
| [STS4](http://phaul.github.io/chess/STS4.epd.html)   | 35     | Square Vacancy							   |
| [STS5](http://phaul.github.io/chess/STS5.epd.html)   | 53     | Bishop vs Knight							   |
| [STS6](http://phaul.github.io/chess/STS6.epd.html)   | 39     | Recapturing								   |
| [STS7](http://phaul.github.io/chess/STS7.epd.html)   | 34     | Simplification							   |
| [STS8](http://phaul.github.io/chess/STS8.epd.html)   | 10     | AKPC										   |
| [STS9](http://phaul.github.io/chess/STS9.epd.html)   | 16     | Advancement of a/b/c pawns				   |
| [STS10](http://phaul.github.io/chess/STS10.epd.html) | 58     | Simplification							   |
| [STS11](http://phaul.github.io/chess/STS11.epd.html) | 14     | King Activity								   |
| [STS12](http://phaul.github.io/chess/STS12.epd.html) | 32     | Center Control							   |
| [STS13](http://phaul.github.io/chess/STS13.epd.html) | 28     | Pawn Play in the Center					   |
| [STS14](http://phaul.github.io/chess/STS14.epd.html) | 42     | 7th Rank                                     |

---

[![Build Status](https://travis-ci.org/phaul/chess.png?branch=master)](https://travis-ci.org/phaul/chess)
