module Rartional

    type rational

    val initRational : int -> int -> rational

    val (.+.) : rational -> rational -> rational
    val (.-.) : rational -> rational -> rational
    val (.*.) : rational -> rational -> rational
    val (./.) : rational -> rational -> rational
