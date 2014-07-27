chess
=====

An other UCI engine written in Haskell


This program is still in an early stage, but we managed to beat pyChess at its strongest setting.

The game PGN:

~~~
[Event "Local Event"]
[Site "Local Site"]
[Date "2014.02.22"]
[Round "1"]
[White "Chess"]
[Black "PyChess 0.10.1"]
[Result "1-0"]
[TimeControl "300+0"]
[Time "11:58:00"]
[WhiteClock "23:49:50.496"]
[BlackClock "0:02:34.911"]
[PlyCount "57"]

1. Nc3 c5 2. Nf3 e6 3. e4 d6 4. Bb5+ Ke7 5. d4 cxd4 6. Qxd4 Nf6 7. e5 dxe5 8.
Qc5+ Qd6 9. Qxc8 Qd8 10. Qxb7+ Nbd7 11. Bxd7 Rb8 12. Qxa7 Ra8 13. Qc5+ Kxd7 14.
Qxe5 Bd6 15. Qd4 Kc8 16. Bg5 h6 17. Bxf6 gxf6 18. O-O-O Bb8 19. Qe3 Qg8 20.
Qc5+ Bc7 21. Nb5 Kb7 22. Qxc7+ Ka6 23. c4 Qf8 24. Rd6+ Qxd6 25. Nxd6 Rhb8 26.
Ne4 Rb6 27. Nd4 Rab8 28. Nc5+ Ka5 29. Nc6# 1-0
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
