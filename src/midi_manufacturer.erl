%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2024, Tony Rogvall
%%% @doc
%%%	MIDI Manufacturer codes
%%% @end
%%% Created : 20 Sep 2024 by Tony Rogvall <tony@rogvall.se>

-module(midi_manufacturer).

%% https://midi.org/sysexidtable

-define(CODE(H0,Name), <<(H0)>> => (Name)).
-define(CODE(H0,H1,H2,Name), <<H0,H1,H2>> => (Name)).

-export([codes/0]).
-export([lookup/1]).

lookup(ID) when is_binary(ID) ->
    maps:get(ID, codes(), "Unknown");
lookup(ID) when is_integer(ID), ID >= 0, ID =< 127 ->
    maps:get(<<ID:8>>, codes());
lookup(ID) when is_integer(ID), ID >= 0, ID =< 16383 ->
    maps:get(<<ID:16>>, codes()).


codes() ->
    #{
      %%  Assigned Manufacturer MIDI SysEx ID Numbers 
      ?CODE(16#00,  "[Used for ID Extensions]"),
      ?CODE(16#01, "Sequential Circuits"),
      ?CODE(16#02, "IDP"),
      ?CODE(16#03, "Voyetra Turtle Beach, Inc."),
      ?CODE(16#04, "Moog Music"),
      ?CODE(16#05, "Passport Designs"),
      ?CODE(16#06, "Lexicon Inc."),
      ?CODE(16#07, "Kurzweil / Young Chang"),
      ?CODE(16#08, "Fender"),
      ?CODE(16#09, "MIDI9"),
      ?CODE(16#0A, "AKG Acoustics"),
      ?CODE(16#0B, "Voyce Music"),
      ?CODE(16#0C, "WaveFrame (Timeline)"),
      ?CODE(16#0D, "ADA Signal Processors, Inc."),
      ?CODE(16#0E, "Garfield Electronics"),
      ?CODE(16#0F, "Ensoniq"),
      ?CODE(16#10, "Oberheim"),
      ?CODE(16#11, "Apple"),
      ?CODE(16#12, "Grey Matter Response"),
      ?CODE(16#13, "Digidesign Inc."),
      ?CODE(16#14, "Palmtree Instruments"),
      ?CODE(16#15, "JLCooper Electronics"),
      ?CODE(16#16, "Lowrey Organ Company"),
      ?CODE(16#17, "Adams-Smith"),
      ?CODE(16#18, "E-mu"),
      ?CODE(16#19, "Harmony Systems"),
      ?CODE(16#1A, "ART"),
      ?CODE(16#1B, "Baldwin"),
      ?CODE(16#1C, "Eventide"),
      ?CODE(16#1D, "Inventronics"),
      ?CODE(16#1E, "Key Concepts"),
      ?CODE(16#1F, "Clarity"),
      ?CODE(16#20, "Passac"),
      ?CODE(16#21, "Proel Labs (SIEL)"),
      ?CODE(16#22, "Synthaxe (UK)"),
      ?CODE(16#23, "Stepp"),
      ?CODE(16#24, "Hohner"),
      ?CODE(16#25, "Twister"),
      ?CODE(16#26, "Ketron s.r.l."),
      ?CODE(16#27, "Jellinghaus MS"),
      ?CODE(16#28, "Southworth Music Systems"),
      ?CODE(16#29, "PPG (Germany)"),
      ?CODE(16#2A, "CESYG Ltd."),
      ?CODE(16#2B, "Solid State Logic Organ Systems"),
      ?CODE(16#2C, "Audio Veritrieb-P. Struven"),
      ?CODE(16#2D, "Neve"),
      ?CODE(16#2E, "Soundtracs Ltd."),
      ?CODE(16#2F, "Elka"),
      ?CODE(16#30, "Dynacord"),
      ?CODE(16#31, "Viscount International Spa (Intercontinental Electronics)"),
      ?CODE(16#32, "Drawmer"),
      ?CODE(16#33, "Clavia Digital Instruments"),
      ?CODE(16#34, "Audio Architecture"),
      ?CODE(16#35, "Generalmusic Corp SpA"),
      ?CODE(16#36, "Cheetah Marketing"),
      ?CODE(16#37, "C.T.M."),
      ?CODE(16#38, "Simmons UK"),
      ?CODE(16#39, "Soundcraft Electronics"),
      ?CODE(16#3A, "Steinberg Media Technologies GmbH"),
      ?CODE(16#3B, "Wersi Gmbh"),
      ?CODE(16#3C, "AVAB Niethammer AB"),
      ?CODE(16#3D, "Digigram"),
      ?CODE(16#3E, "Waldorf Electronics GmbH"),
      ?CODE(16#3F, "Quasimidi"),
      %% 40H to 5FH 	[Assigned by AMEI for Japanese Manufacturers]
      %% 60H to 7FH 	[Reserved for Other Uses]
      ?CODE(16#00, 16#00, 16#01, "Time/Warner Interactive"),
      ?CODE(16#00, 16#00, 16#02, "Advanced Gravis Comp. Tech Ltd."),
      ?CODE(16#00, 16#00, 16#03, "Media Vision"),
      ?CODE(16#00, 16#00, 16#04, "Dornes Research Group"),
      ?CODE(16#00, 16#00, 16#05, "K-Muse"),
      ?CODE(16#00, 16#00, 16#06, "Stypher"),
      ?CODE(16#00, 16#00, 16#07, "Digital Music Corp."),
      ?CODE(16#00, 16#00, 16#08, "IOTA Systems"),
      ?CODE(16#00, 16#00, 16#09, "New England Digital"),
      ?CODE(16#00, 16#00, 16#0A, "Artisyn"),
      ?CODE(16#00, 16#00, 16#0B, "IVL Technologies Ltd."),
      ?CODE(16#00, 16#00, 16#0C, "Southern Music Systems"),
      ?CODE(16#00, 16#00, 16#0D, "Lake Butler Sound Company"),
      ?CODE(16#00, 16#00, 16#0E, "Alesis Studio Electronics"),
      ?CODE(16#00, 16#00, 16#0F, "Sound Creation"),
      ?CODE(16#00, 16#00, 16#10, "DOD Electronics Corp."),
      ?CODE(16#00, 16#00, 16#11, "Studer-Editech"),
      ?CODE(16#00, 16#00, 16#12, "Sonus"),
      ?CODE(16#00, 16#00, 16#13, "Temporal Acuity Products"),
      ?CODE(16#00, 16#00, 16#14, "Perfect Fretworks"),
      ?CODE(16#00, 16#00, 16#15, "KAT Inc."),
      ?CODE(16#00, 16#00, 16#16, "Opcode Systems"),
      ?CODE(16#00, 16#00, 16#17, "Rane Corporation"),
      ?CODE(16#00, 16#00, 16#18, "Anadi Electronique"),
      ?CODE(16#00, 16#00, 16#19, "KMX"),
      ?CODE(16#00, 16#00, 16#1A, "Allen & Heath Brenell"),
      ?CODE(16#00, 16#00, 16#1B, "Peavey Electronics"),
      ?CODE(16#00, 16#00, 16#1C, "360 Systems"),
      ?CODE(16#00, 16#00, 16#1D, "Spectrum Design and Development"),
      ?CODE(16#00, 16#00, 16#1E, "Marquis Music"),
      ?CODE(16#00, 16#00, 16#1F, "Zeta Systems"),
      ?CODE(16#00, 16#00, 16#20, "Axxes (Brian Parsonett)"),
      ?CODE(16#00, 16#00, 16#21, "Orban"),
      ?CODE(16#00, 16#00, 16#22, "Indian Valley Mfg."),
      ?CODE(16#00, 16#00, 16#23, "Triton"),
      ?CODE(16#00, 16#00, 16#24, "KTI"),
      ?CODE(16#00, 16#00, 16#25, "Breakaway Technologies"),
      ?CODE(16#00, 16#00, 16#26, "Leprecon / CAE Inc."),
      ?CODE(16#00, 16#00, 16#27, "Harrison Systems Inc."),
      ?CODE(16#00, 16#00, 16#28, "Future Lab/Mark Kuo"),
      ?CODE(16#00, 16#00, 16#29, "Rocktron Corporation"),
      ?CODE(16#00, 16#00, 16#2A, "PianoDisc"),
      ?CODE(16#00, 16#00, 16#2B, "Cannon Research Group"),
      ?CODE(16#00, 16#00, 16#2C, "Reserved"),
      ?CODE(16#00, 16#00, 16#2D, "Rodgers Instrument LLC"),
      ?CODE(16#00, 16#00, 16#2E, "Blue Sky Logic"),
      ?CODE(16#00, 16#00, 16#2F, "Encore Electronics"),
      ?CODE(16#00, 16#00, 16#30, "Uptown"),
      ?CODE(16#00, 16#00, 16#31, "Voce"),
      ?CODE(16#00, 16#00, 16#32, "CTI Audio, Inc. (Musically Intel. Devs.)"),
      ?CODE(16#00, 16#00, 16#33, "S3 Incorporated"),
      ?CODE(16#00, 16#00, 16#34, "Broderbund / Red Orb"),
      ?CODE(16#00, 16#00, 16#35, "Allen Organ Co."),
      ?CODE(16#00, 16#00, 16#36, "Reserved"),
      ?CODE(16#00, 16#00, 16#37, "Music Quest"),
      ?CODE(16#00, 16#00, 16#38, "Aphex"),
      ?CODE(16#00, 16#00, 16#39, "Gallien Krueger"),
      ?CODE(16#00, 16#00, 16#3A, "IBM"),
      ?CODE(16#00, 16#00, 16#3B, "Mark Of The Unicorn"),
      ?CODE(16#00, 16#00, 16#3C, "Hotz Corporation"),
      ?CODE(16#00, 16#00, 16#3D, "ETA Lighting"),
      ?CODE(16#00, 16#00, 16#3E, "NSI Corporation"),
      ?CODE(16#00, 16#00, 16#3F, "Ad Lib, Inc."),
      ?CODE(16#00, 16#00, 16#40, "Richmond Sound Design"),
      ?CODE(16#00, 16#00, 16#41, "Microsoft"),
      ?CODE(16#00, 16#00, 16#42, "Mindscape (Software Toolworks)"),
      ?CODE(16#00, 16#00, 16#43, "Russ Jones Marketing / Niche"),
      ?CODE(16#00, 16#00, 16#44, "Intone"),
      ?CODE(16#00, 16#00, 16#45, "Advanced Remote Technologies"),
      ?CODE(16#00, 16#00, 16#46, "White Instruments"),
      ?CODE(16#00, 16#00, 16#47, "GT Electronics/Groove Tubes"),
      ?CODE(16#00, 16#00, 16#48, "Pacific Research & Engineering"),
      ?CODE(16#00, 16#00, 16#49, "Timeline Vista, Inc."),
      ?CODE(16#00, 16#00, 16#4A, "Mesa Boogie Ltd."),
      ?CODE(16#00, 16#00, 16#4B, "FSLI"),
      ?CODE(16#00, 16#00, 16#4C, "Sequoia Development Group"),
      ?CODE(16#00, 16#00, 16#4D, "Studio Electronics"),
      ?CODE(16#00, 16#00, 16#4E, "Euphonix, Inc"),
      ?CODE(16#00, 16#00, 16#4F, "InterMIDI, Inc."),
      ?CODE(16#00, 16#00, 16#50, "MIDI Solutions Inc."),
      ?CODE(16#00, 16#00, 16#51, "3DO Company"),
      ?CODE(16#00, 16#00, 16#52, "Lightwave Research / High End Systems"),
      ?CODE(16#00, 16#00, 16#53, "Micro-W Corporation"),
      ?CODE(16#00, 16#00, 16#54, "Spectral Synthesis, Inc."),
      ?CODE(16#00, 16#00, 16#55, "Lone Wolf"),
      ?CODE(16#00, 16#00, 16#56, "Studio Technologies Inc."),
      ?CODE(16#00, 16#00, 16#57, "Peterson Electro-Musical Product, Inc."),
      ?CODE(16#00, 16#00, 16#58, "Atari Corporation"),
      ?CODE(16#00, 16#00, 16#59, "Marion Systems Corporation"),
      ?CODE(16#00, 16#00, 16#5A, "Design Event"),
      ?CODE(16#00, 16#00, 16#5B, "Winjammer Software Ltd."),
      ?CODE(16#00, 16#00, 16#5C, "AT&T Bell Laboratories"),
      ?CODE(16#00, 16#00, 16#5D, "Reserved"),
      ?CODE(16#00, 16#00, 16#5E, "Symetrix"),
      ?CODE(16#00, 16#00, 16#5F, "MIDI the World"),
      ?CODE(16#00, 16#00, 16#60, "Spatializer"),
      ?CODE(16#00, 16#00, 16#61, "Micros ‘N MIDI"),
      ?CODE(16#00, 16#00, 16#62, "Accordians International"),
      ?CODE(16#00, 16#00, 16#63, "EuPhonics (now 3Com)"),
      ?CODE(16#00, 16#00, 16#64, "Musonix"),
      ?CODE(16#00, 16#00, 16#65, "Turtle Beach Systems (Voyetra)"),
      ?CODE(16#00, 16#00, 16#66, "Loud Technologies / Mackie"),
      ?CODE(16#00, 16#00, 16#67, "Compuserve"),
      ?CODE(16#00, 16#00, 16#68, "BEC Technologies"),
      ?CODE(16#00, 16#00, 16#69, "QRS Music Inc"),
      ?CODE(16#00, 16#00, 16#6A, "P.G. Music"),
      ?CODE(16#00, 16#00, 16#6B, "Sierra Semiconductor"),
      ?CODE(16#00, 16#00, 16#6C, "EpiGraf"),
      ?CODE(16#00, 16#00, 16#6D, "Electronics Diversified Inc"),
      ?CODE(16#00, 16#00, 16#6E, "Tune 1000"),
      ?CODE(16#00, 16#00, 16#6F, "Advanced Micro Devices"),
      ?CODE(16#00, 16#00, 16#70, "Mediamation"),
      ?CODE(16#00, 16#00, 16#71, "Sabine Musical Mfg. Co. Inc."),
      ?CODE(16#00, 16#00, 16#72, "Woog Labs"),
      ?CODE(16#00, 16#00, 16#73, "Micropolis Corp"),
      ?CODE(16#00, 16#00, 16#74, "Ta Horng Musical Instrument"),
      ?CODE(16#00, 16#00, 16#75, "e-Tek Labs (Forte Tech)"),
      ?CODE(16#00, 16#00, 16#76, "Electro-Voice"),
      ?CODE(16#00, 16#00, 16#77, "Midisoft Corporation"),
      ?CODE(16#00, 16#00, 16#78, "QSound Labs"),
      ?CODE(16#00, 16#00, 16#79, "Westrex"),
      ?CODE(16#00, 16#00, 16#7A, "Nvidia"),
      ?CODE(16#00, 16#00, 16#7B, "ESS Technology"),
      ?CODE(16#00, 16#00, 16#7C, "Media Trix Peripherals"),
      ?CODE(16#00, 16#00, 16#7D, "Brooktree Corp"),
      ?CODE(16#00, 16#00, 16#7E, "Otari Corp"),
      ?CODE(16#00, 16#00, 16#7F, "Key Electronics, Inc."),
      ?CODE(16#00, 16#01, 16#00, "Shure Incorporated"),
      ?CODE(16#00, 16#01, 16#01, "AuraSound"),
      ?CODE(16#00, 16#01, 16#02, "Crystal Semiconductor"),
      ?CODE(16#00, 16#01, 16#03, "Conexant (Rockwell)"),
      ?CODE(16#00, 16#01, 16#04, "Silicon Graphics"),
      ?CODE(16#00, 16#01, 16#05, "M-Audio (Midiman)"),
      ?CODE(16#00, 16#01, 16#06, "PreSonus"),
      ?CODE(16#00, 16#01, 16#08, "Topaz Enterprises"),
      ?CODE(16#00, 16#01, 16#09, "Cast Lighting"),
      ?CODE(16#00, 16#01, 16#0A, "Microsoft Consumer Division"),
      ?CODE(16#00, 16#01, 16#0B, "Sonic Foundry"),
      ?CODE(16#00, 16#01, 16#0C, "Line 6 (Fast Forward) (Yamaha)"),
      ?CODE(16#00, 16#01, 16#0D, "Beatnik Inc"),
      ?CODE(16#00, 16#01, 16#0E, "Van Koevering Company"),
      ?CODE(16#00, 16#01, 16#0F, "Altech Systems"),
      ?CODE(16#00, 16#01, 16#10, "S & S Research"),
      ?CODE(16#00, 16#01, 16#11, "VLSI Technology"),
      ?CODE(16#00, 16#01, 16#12, "Chromatic Research"),
      ?CODE(16#00, 16#01, 16#13, "Sapphire"),
      ?CODE(16#00, 16#01, 16#14, "IDRC"),
      ?CODE(16#00, 16#01, 16#15, "Justonic Tuning"),
      ?CODE(16#00, 16#01, 16#16, "TorComp Research Inc."),
      ?CODE(16#00, 16#01, 16#17, "Newtek Inc."),
      ?CODE(16#00, 16#01, 16#18, "Sound Sculpture"),
      ?CODE(16#00, 16#01, 16#19, "Walker Technical"),
      ?CODE(16#00, 16#01, 16#1A, "Digital Harmony (PAVO)"),
      ?CODE(16#00, 16#01, 16#1B, "InVision Interactive"),
      ?CODE(16#00, 16#01, 16#1C, "T-Square Design"),
      ?CODE(16#00, 16#01, 16#1D, "Nemesys Music Technology"),
      ?CODE(16#00, 16#01, 16#1E, "DBX Professional (Harman Intl)"),
      ?CODE(16#00, 16#01, 16#1F, "Syndyne Corporation"),
      ?CODE(16#00, 16#01, 16#20, "Bitheadz"),
      ?CODE(16#00, 16#01, 16#21, "BandLab Technologies"),
      ?CODE(16#00, 16#01, 16#22, "Analog Devices"),
      ?CODE(16#00, 16#01, 16#23, "National Semiconductor"),
      ?CODE(16#00, 16#01, 16#24, "Boom Theory / Adinolfi Alternative Percussion"),
      ?CODE(16#00, 16#01, 16#25, "Virtual DSP Corporation"),
      ?CODE(16#00, 16#01, 16#26, "Antares Systems"),
      ?CODE(16#00, 16#01, 16#27, "Angel Software"),
      ?CODE(16#00, 16#01, 16#28, "St Louis Music"),
      ?CODE(16#00, 16#01, 16#29, "Passport Music Software LLC (Gvox)"),
      ?CODE(16#00, 16#01, 16#2A, "Ashley Audio Inc."),
      ?CODE(16#00, 16#01, 16#2B, "Vari-Lite Inc."),
      ?CODE(16#00, 16#01, 16#2C, "Summit Audio Inc."),
      ?CODE(16#00, 16#01, 16#2D, "Aureal Semiconductor Inc."),
      ?CODE(16#00, 16#01, 16#2E, "SeaSound LLC"),
      ?CODE(16#00, 16#01, 16#2F, "U.S. Robotics"),
      ?CODE(16#00, 16#01, 16#30, "Aurisis Research"),
      ?CODE(16#00, 16#01, 16#31, "Nearfield Research"),
      ?CODE(16#00, 16#01, 16#32, "FM7 Inc"),
      ?CODE(16#00, 16#01, 16#33, "Swivel Systems"),
      ?CODE(16#00, 16#01, 16#34, "Hyperactive Audio Systems"),
      ?CODE(16#00, 16#01, 16#35, "MidiLite (Castle Studios Productions)"),
      ?CODE(16#00, 16#01, 16#36, "Radikal Technologies"),
      ?CODE(16#00, 16#01, 16#37, "Roger Linn Design"),
      ?CODE(16#00, 16#01, 16#38, "TC-Helicon Vocal Technologies"),
      ?CODE(16#00, 16#01, 16#39, "Event Electronics"),
      ?CODE(16#00, 16#01, 16#3A, "Sonic Network Inc"),
      ?CODE(16#00, 16#01, 16#3B, "Realtime Music Solutions"),
      ?CODE(16#00, 16#01, 16#3C, "Apogee Digital"),
      ?CODE(16#00, 16#01, 16#3D, "Classical Organs, Inc."),
      ?CODE(16#00, 16#01, 16#3E, "Microtools Inc."),
      ?CODE(16#00, 16#01, 16#3F, "Numark Industries"),
      ?CODE(16#00, 16#01, 16#40, "Frontier Design Group, LLC"),
      ?CODE(16#00, 16#01, 16#41, "Recordare LLC"),
      ?CODE(16#00, 16#01, 16#42, "Starr Labs"),
      ?CODE(16#00, 16#01, 16#43, "Voyager Sound Inc."),
      ?CODE(16#00, 16#01, 16#44, "Manifold Labs"),
      ?CODE(16#00, 16#01, 16#45, "Aviom Inc."),
      ?CODE(16#00, 16#01, 16#46, "Mixmeister Technology"),
      ?CODE(16#00, 16#01, 16#47, "Notation Software"),
      ?CODE(16#00, 16#01, 16#48, "Mercurial Communications"),
      ?CODE(16#00, 16#01, 16#49, "Wave Arts"),
      ?CODE(16#00, 16#01, 16#4A, "Logic Sequencing Devices"),
      ?CODE(16#00, 16#01, 16#4B, "Axess Electronics"),
      ?CODE(16#00, 16#01, 16#4C, "Muse Research"),
      ?CODE(16#00, 16#01, 16#4D, "Open Labs"),
      ?CODE(16#00, 16#01, 16#4E, "Guillemot Corp"),
      ?CODE(16#00, 16#01, 16#4F, "Samson Technologies"),
      ?CODE(16#00, 16#01, 16#50, "Electronic Theatre Controls"),
      ?CODE(16#00, 16#01, 16#51, "Blackberry (RIM)"),
      ?CODE(16#00, 16#01, 16#52, "Mobileer"),
      ?CODE(16#00, 16#01, 16#53, "Synthogy"),
      ?CODE(16#00, 16#01, 16#54, "Lynx Studio Technology Inc."),
      ?CODE(16#00, 16#01, 16#55, "Damage Control Engineering LLC"),
      ?CODE(16#00, 16#01, 16#56, "Yost Engineering, Inc."),
      ?CODE(16#00, 16#01, 16#57, "Brooks & Forsman Designs LLC / DrumLite"),
      ?CODE(16#00, 16#01, 16#58, "Infinite Response"),
      ?CODE(16#00, 16#01, 16#59, "Garritan Corp"),
      ?CODE(16#00, 16#01, 16#5A, "Plogue Art et Technologie, Inc"),
      ?CODE(16#00, 16#01, 16#5B, "RJM Music Technology"),
      ?CODE(16#00, 16#01, 16#5C, "Custom Solutions Software"),
      ?CODE(16#00, 16#01, 16#5D, "Sonarcana LLC / Highly Liquid"),
      ?CODE(16#00, 16#01, 16#5E, "Centrance"),
      ?CODE(16#00, 16#01, 16#5F, "Kesumo LLC"),
      ?CODE(16#00, 16#01, 16#60, "Stanton (Gibson Brands)"),
      ?CODE(16#00, 16#01, 16#61, "Livid Instruments"),
      ?CODE(16#00, 16#01, 16#62, "First Act / 745 Media"),
      ?CODE(16#00, 16#01, 16#63, "Pygraphics, Inc."),
      ?CODE(16#00, 16#01, 16#64, "Panadigm Innovations Ltd"),
      ?CODE(16#00, 16#01, 16#65, "Avedis Zildjian Co"),
      ?CODE(16#00, 16#01, 16#66, "Auvital Music Corp"),
      ?CODE(16#00, 16#01, 16#67, "You Rock Guitar (was: Inspired Instruments)"),
      ?CODE(16#00, 16#01, 16#68, "Chris Grigg Designs"),
      ?CODE(16#00, 16#01, 16#69, "Slate Digital LLC"),
      ?CODE(16#00, 16#01, 16#6A, "Mixware"),
      ?CODE(16#00, 16#01, 16#6B, "Social Entropy"),
      ?CODE(16#00, 16#01, 16#6C, "Source Audio LLC"),
      ?CODE(16#00, 16#01, 16#6D, "Ernie Ball / Music Man"),
      ?CODE(16#00, 16#01, 16#6E, "Fishman"),
      ?CODE(16#00, 16#01, 16#6F, "Custom Audio Electronics"),
      ?CODE(16#00, 16#01, 16#70, "American Audio/DJ"),
      ?CODE(16#00, 16#01, 16#71, "Mega Control Systems"),
      ?CODE(16#00, 16#01, 16#72, "Kilpatrick Audio"),
      ?CODE(16#00, 16#01, 16#73, "iConnectivity"),
      ?CODE(16#00, 16#01, 16#74, "Fractal Audio"),
      ?CODE(16#00, 16#01, 16#75, "NetLogic Microsystems"),
      ?CODE(16#00, 16#01, 16#76, "Music Computing"),
      ?CODE(16#00, 16#01, 16#77, "Nektar Technology Inc"),
      ?CODE(16#00, 16#01, 16#78, "Zenph Sound Innovations"),
      ?CODE(16#00, 16#01, 16#79, "DJTechTools.com"),
      ?CODE(16#00, 16#01, 16#7A, "Rezonance Labs"),
      ?CODE(16#00, 16#01, 16#7B, "Decibel Eleven"),
      ?CODE(16#00, 16#01, 16#7C, "CNMAT"),
      ?CODE(16#00, 16#01, 16#7D, "Media Overkill"),
      ?CODE(16#00, 16#01, 16#7E, "Confusion Studios"),
      ?CODE(16#00, 16#01, 16#7F, "moForte Inc"),
      ?CODE(16#00, 16#02, 16#00, "Miselu Inc"),
      ?CODE(16#00, 16#02, 16#01, "Amelia’s Compass LLC"),
      ?CODE(16#00, 16#02, 16#02, "Zivix LLC"),
      ?CODE(16#00, 16#02, 16#03, "Artiphon"),
      ?CODE(16#00, 16#02, 16#04, "Synclavier Digital"),
      ?CODE(16#00, 16#02, 16#05, "Light & Sound Control Devices LLC"),
      ?CODE(16#00, 16#02, 16#06, "Retronyms Inc"),
      ?CODE(16#00, 16#02, 16#07, "JS Technologies"),
      ?CODE(16#00, 16#02, 16#08, "Quicco Sound"),
      ?CODE(16#00, 16#02, 16#09, "A-Designs Audio"),
      ?CODE(16#00, 16#02, 16#0A, "McCarthy Music Corp"),
      ?CODE(16#00, 16#02, 16#0B, "Denon DJ"),
      ?CODE(16#00, 16#02, 16#0C, "Keith Robert Murray"),
      ?CODE(16#00, 16#02, 16#0D, "Google"),
      ?CODE(16#00, 16#02, 16#0E, "ISP Technologies"),
      ?CODE(16#00, 16#02, 16#0F, "Abstrakt Instruments LLC"),
      ?CODE(16#00, 16#02, 16#10, "Meris LLC"),
      ?CODE(16#00, 16#02, 16#11, "Sensorpoint LLC"),
      ?CODE(16#00, 16#02, 16#12, "Hi-Z Labs"),
      ?CODE(16#00, 16#02, 16#13, "Imitone"),
      ?CODE(16#00, 16#02, 16#14, "Intellijel Designs Inc."),
      ?CODE(16#00, 16#02, 16#15, "Dasz Instruments Inc."),
      ?CODE(16#00, 16#02, 16#16, "Remidi"),
      ?CODE(16#00, 16#02, 16#17, "Disaster Area Designs LLC"),
      ?CODE(16#00, 16#02, 16#18, "Universal Audio"),
      ?CODE(16#00, 16#02, 16#19, "Carter Duncan Corp"),
      ?CODE(16#00, 16#02, 16#1A, "Essential Technology"),
      ?CODE(16#00, 16#02, 16#1B, "Cantux Research LLC"),
      ?CODE(16#00, 16#02, 16#1C, "Hummel Technologies"),
      ?CODE(16#00, 16#02, 16#1D, "Sensel Inc"),
      ?CODE(16#00, 16#02, 16#1E, "DBML Group"),
      ?CODE(16#00, 16#02, 16#1F, "Madrona Labs"),
      ?CODE(16#00, 16#02, 16#20, "Mesa Boogie"),
      ?CODE(16#00, 16#02, 16#21, "Effigy Labs"),
      ?CODE(16#00, 16#02, 16#22, "Amenote"),
      ?CODE(16#00, 16#02, 16#23, "Red Panda LLC"),
      ?CODE(16#00, 16#02, 16#24, "OnSong LLC"),
      ?CODE(16#00, 16#02, 16#25, "Jamboxx Inc."),
      ?CODE(16#00, 16#02, 16#26, "Electro-Harmonix "),
      ?CODE(16#00, 16#02, 16#27, "RnD64 Inc"),
      ?CODE(16#00, 16#02, 16#28, "Neunaber Technology LLC "),
      ?CODE(16#00, 16#02, 16#29, "Kaom Inc."),
      ?CODE(16#00, 16#02, 16#2A, "Hallowell EMC"),
      ?CODE(16#00, 16#02, 16#2B, "Sound Devices, LLC"),
      ?CODE(16#00, 16#02, 16#2C, "Spectrasonics, Inc"),
      ?CODE(16#00, 16#02, 16#2D, "Second Sound, LLC"),
      ?CODE(16#00, 16#02, 16#2E, "8eo (Horn)"),
      ?CODE(16#00, 16#02, 16#2F, "VIDVOX LLC"),
      ?CODE(16#00, 16#02, 16#30, "Matthews Effects"),
      ?CODE(16#00, 16#02, 16#31, "Bright Blue Beetle"),
      ?CODE(16#00, 16#02, 16#32, "Audio Impressions"),
      ?CODE(16#00, 16#02, 16#33, "Looperlative"),
      ?CODE(16#00, 16#02, 16#34, "Steinway"),
      ?CODE(16#00, 16#02, 16#35, "Ingenious Arts and Technologies LLC"),
      ?CODE(16#00, 16#02, 16#36, "DCA Audio"),
      ?CODE(16#00, 16#02, 16#37, "Buchla USA"),
      ?CODE(16#00, 16#02, 16#38, "Sinicon"),
      ?CODE(16#00, 16#02, 16#39, "Isla Instruments"),
      ?CODE(16#00, 16#02, 16#3A, "Soundiron LLC"),
      ?CODE(16#00, 16#02, 16#3B, "Sonoclast, LLC"),
      ?CODE(16#00, 16#02, 16#3C, "Copper and Cedar"),
      ?CODE(16#00, 16#02, 16#3D, "Whirled Notes"),
      ?CODE(16#00, 16#02, 16#3E, "Cejetvole, LLC"),
      ?CODE(16#00, 16#02, 16#3F, "DAWn Audio LLC"),
      ?CODE(16#00, 16#02, 16#40, "Space Brain Circuits"),
      ?CODE(16#00, 16#02, 16#41, "Caedence "),
      ?CODE(16#00, 16#02, 16#42, "HCN Designs, LLC (The MIDI Maker)"),
      ?CODE(16#00, 16#02, 16#43, "PTZOptics"),
      ?CODE(16#00, 16#02, 16#44, "Noise Engineering"),
      ?CODE(16#00, 16#02, 16#45, "Synthesia LLC"),
      ?CODE(16#00, 16#02, 16#46, "Jeff Whitehead Lutherie LLC"),
      ?CODE(16#00, 16#02, 16#47, "Wampler Pedals Inc."),
      ?CODE(16#00, 16#02, 16#48, "Tapis Magique"),
      ?CODE(16#00, 16#02, 16#49, "Leaf Secrets"),
      ?CODE(16#00, 16#02, 16#4A, "Groove Synthesis"),
      ?CODE(16#00, 16#02, 16#4B, "Audiocipher Technologies LLC"),
      ?CODE(16#00, 16#02, 16#4C, "Mellotron Inc."),
      ?CODE(16#00, 16#02, 16#4D, "Hologram Electronics LLC"),
      ?CODE(16#00, 16#02, 16#4E, "iCON Americas, LLC"),
      ?CODE(16#00, 16#02, 16#4F, "Singular Sound"),
      ?CODE(16#00, 16#02, 16#50, "Genovation Inc"),
      ?CODE(16#00, 16#02, 16#51, "Method Red"),
      ?CODE(16#00, 16#02, 16#52, "Brain Inventions"),
      ?CODE(16#00, 16#02, 16#53, "Synervoz Communications Inc."),
      ?CODE(16#00, 16#02, 16#54, "Hypertriangle Inc"),

      %% European & Asian Group
      ?CODE(16#00, 16#20, 16#00, "Dream SAS"),
      ?CODE(16#00, 16#20, 16#01, "Strand Lighting"),
      ?CODE(16#00, 16#20, 16#02, "Amek Div of Harman Industries"),
      ?CODE(16#00, 16#20, 16#03, "Casa Di Risparmio Di Loreto"),
      ?CODE(16#00, 16#20, 16#04, "Böhm electronic GmbH"),
      ?CODE(16#00, 16#20, 16#05, "Syntec Digital Audio"),
      ?CODE(16#00, 16#20, 16#06, "Trident Audio Developments"),
      ?CODE(16#00, 16#20, 16#07, "Real World Studio"),
      ?CODE(16#00, 16#20, 16#08, "Evolution Synthesis, Ltd"),
      ?CODE(16#00, 16#20, 16#09, "Yes Technology"),
      ?CODE(16#00, 16#20, 16#0A, "Audiomatica"),
      ?CODE(16#00, 16#20, 16#0B, "Bontempi SpA (Sigma)"),
      ?CODE(16#00, 16#20, 16#0C, "F.B.T. Elettronica SpA"),
      ?CODE(16#00, 16#20, 16#0D, "MidiTemp GmbH"),
      ?CODE(16#00, 16#20, 16#0E, "LA Audio (Larking Audio)"),
      ?CODE(16#00, 16#20, 16#0F, "Zero 88 Lighting Limited"),
      ?CODE(16#00, 16#20, 16#10, "Micon Audio Electronics GmbH"),
      ?CODE(16#00, 16#20, 16#11, "Forefront Technology"),
      ?CODE(16#00, 16#20, 16#12, "Studio Audio and Video Ltd."),
      ?CODE(16#00, 16#20, 16#13, "Kenton Electronics"),
      ?CODE(16#00, 16#20, 16#14, "Celco/ Electrosonic"),
      ?CODE(16#00, 16#20, 16#15, "ADB"),
      ?CODE(16#00, 16#20, 16#16, "Marshall Products Limited"),
      ?CODE(16#00, 16#20, 16#17, "DDA"),
      ?CODE(16#00, 16#20, 16#18, "BSS Audio Ltd."),
      ?CODE(16#00, 16#20, 16#19, "MA Lighting Technology"),
      ?CODE(16#00, 16#20, 16#1A, "Fatar SRL c/o Music Industries"),
      ?CODE(16#00, 16#20, 16#1B, "QSC Audio Products Inc."),
      ?CODE(16#00, 16#20, 16#1C, "Artisan Clasic Organ Inc."),
      ?CODE(16#00, 16#20, 16#1D, "Orla Spa"),
      ?CODE(16#00, 16#20, 16#1E, "Pinnacle Audio (Klark Teknik PLC)"),
      ?CODE(16#00, 16#20, 16#1F, "TC Electronics"),
      ?CODE(16#00, 16#20, 16#20, "Doepfer Musikelektronik GmbH"),
      ?CODE(16#00, 16#20, 16#21, "Creative ATC / E-mu"),
      ?CODE(16#00, 16#20, 16#22, "Seyddo/Minami"),
      ?CODE(16#00, 16#20, 16#23, "LG Electronics (Goldstar)"),
      ?CODE(16#00, 16#20, 16#24, "Midisoft sas di M.Cima & C"),
      ?CODE(16#00, 16#20, 16#25, "Samick Musical Inst. Co. Ltd."),
      ?CODE(16#00, 16#20, 16#26, "Penny and Giles (Bowthorpe PLC)"),
      ?CODE(16#00, 16#20, 16#27, "Acorn Computer"),
      ?CODE(16#00, 16#20, 16#28, "LSC Electronics Pty. Ltd."),
      ?CODE(16#00, 16#20, 16#29, "Focusrite/Novation"),
      ?CODE(16#00, 16#20, 16#2A, "Samkyung Mechatronics"),
      ?CODE(16#00, 16#20, 16#2B, "Medeli Electronics Co."),
      ?CODE(16#00, 16#20, 16#2C, "Charlie Lab SRL"),
      ?CODE(16#00, 16#20, 16#2D, "Blue Chip Music Technology"),
      ?CODE(16#00, 16#20, 16#2E, "BEE OH Corp"),
      ?CODE(16#00, 16#20, 16#2F, "LG Semicon America"),
      ?CODE(16#00, 16#20, 16#30, "TESI"),
      ?CODE(16#00, 16#20, 16#31, "EMAGIC"),
      ?CODE(16#00, 16#20, 16#32, "Behringer GmbH"),
      ?CODE(16#00, 16#20, 16#33, "Access Music Electronics"),
      ?CODE(16#00, 16#20, 16#34, "Synoptic"),
      ?CODE(16#00, 16#20, 16#35, "Hanmesoft"),
      ?CODE(16#00, 16#20, 16#36, "Terratec Electronic GmbH"),
      ?CODE(16#00, 16#20, 16#37, "Proel SpA"),
      ?CODE(16#00, 16#20, 16#38, "IBK MIDI"),
      ?CODE(16#00, 16#20, 16#39, "IRCAM"),
      ?CODE(16#00, 16#20, 16#3A, "Propellerhead Software"),
      ?CODE(16#00, 16#20, 16#3B, "Red Sound Systems Ltd"),
      ?CODE(16#00, 16#20, 16#3C, "Elektron ESI AB"),
      ?CODE(16#00, 16#20, 16#3D, "Sintefex Audio"),
      ?CODE(16#00, 16#20, 16#3E, "MAM (Music and More)"),
      ?CODE(16#00, 16#20, 16#3F, "Amsaro GmbH"),
      ?CODE(16#00, 16#20, 16#40, "CDS Advanced Technology BV (Lanbox)"),
      ?CODE(16#00, 16#20, 16#41, "Mode Machines (Touched By Sound GmbH)"),
      ?CODE(16#00, 16#20, 16#42, "DSP Arts"),
      ?CODE(16#00, 16#20, 16#43, "Phil Rees Music Tech"),
      ?CODE(16#00, 16#20, 16#44, "Stamer Musikanlagen GmbH"),
      ?CODE(16#00, 16#20, 16#45, "Musical Muntaner S.A. dba Soundart"),
      ?CODE(16#00, 16#20, 16#46, "C-Mexx Software"),
      ?CODE(16#00, 16#20, 16#47, "Klavis Technologies"),
      ?CODE(16#00, 16#20, 16#48, "Noteheads AB"),
      ?CODE(16#00, 16#20, 16#49, "Algorithmix"),
      ?CODE(16#00, 16#20, 16#4A, "Skrydstrup R&D"),
      ?CODE(16#00, 16#20, 16#4B, "Professional Audio Company"),
      ?CODE(16#00, 16#20, 16#4C, "NewWave Labs (MadWaves)"),
      ?CODE(16#00, 16#20, 16#4D, "Vermona"),
      ?CODE(16#00, 16#20, 16#4E, "Nokia"),
      ?CODE(16#00, 16#20, 16#4F, "Wave Idea"),
      ?CODE(16#00, 16#20, 16#50, "Hartmann GmbH"),
      ?CODE(16#00, 16#20, 16#51, "Lion’s Tracs"),
      ?CODE(16#00, 16#20, 16#52, "Analogue Systems"),
      ?CODE(16#00, 16#20, 16#53, "Focal-JMlab"),
      ?CODE(16#00, 16#20, 16#54, "Ringway Electronics (Chang-Zhou) Co Ltd"),
      ?CODE(16#00, 16#20, 16#55, "Faith Technologies (Digiplug)"),
      ?CODE(16#00, 16#20, 16#56, "Showworks"),
      ?CODE(16#00, 16#20, 16#57, "Manikin Electronic"),
      ?CODE(16#00, 16#20, 16#58, "1 Come Tech"),
      ?CODE(16#00, 16#20, 16#59, "Phonic Corp"),
      ?CODE(16#00, 16#20, 16#5A, "Dolby Australia (Lake)"),
      ?CODE(16#00, 16#20, 16#5B, "Silansys Technologies"),
      ?CODE(16#00, 16#20, 16#5C, "Winbond Electronics"),
      ?CODE(16#00, 16#20, 16#5D, "Cinetix Medien und Interface GmbH"),
      ?CODE(16#00, 16#20, 16#5E, "A&G Soluzioni Digitali"),
      ?CODE(16#00, 16#20, 16#5F, "Sequentix GmbH"),
      ?CODE(16#00, 16#20, 16#60, "Oram Pro Audio"),
      ?CODE(16#00, 16#20, 16#61, "Be4 Ltd"),
      ?CODE(16#00, 16#20, 16#62, "Infection Music"),
      ?CODE(16#00, 16#20, 16#63, "Central Music Co. (CME)"),
      ?CODE(16#00, 16#20, 16#64, "genoQs Machines GmbH"),
      ?CODE(16#00, 16#20, 16#65, "Medialon"),
      ?CODE(16#00, 16#20, 16#66, "Waves Audio Ltd"),
      ?CODE(16#00, 16#20, 16#67, "Jerash Labs"),
      ?CODE(16#00, 16#20, 16#68, "Da Fact"),
      ?CODE(16#00, 16#20, 16#69, "Elby Designs"),
      ?CODE(16#00, 16#20, 16#6A, "Spectral Audio"),
      ?CODE(16#00, 16#20, 16#6B, "Arturia"),
      ?CODE(16#00, 16#20, 16#6C, "Vixid"),
      ?CODE(16#00, 16#20, 16#6D, "C-Thru Music"),
      ?CODE(16#00, 16#20, 16#6E, "Ya Horng Electronic Co LTD"),
      ?CODE(16#00, 16#20, 16#6F, "SM Pro Audio"),
      ?CODE(16#00, 16#20, 16#70, "OTO Machines"),
      ?CODE(16#00, 16#20, 16#71, "ELZAB S.A. (G LAB)"),
      ?CODE(16#00, 16#20, 16#72, "Blackstar Amplification Ltd"),
      ?CODE(16#00, 16#20, 16#73, "M3i Technologies GmbH"),
      ?CODE(16#00, 16#20, 16#74, "Gemalto (from Xiring)"),
      ?CODE(16#00, 16#20, 16#75, "Prostage SL"),
      ?CODE(16#00, 16#20, 16#76, "Teenage Engineering"),
      ?CODE(16#00, 16#20, 16#77, "Tobias Erichsen Consulting"),
      ?CODE(16#00, 16#20, 16#78, "Nixer Ltd"),
      ?CODE(16#00, 16#20, 16#79, "Hanpin Electron Co Ltd"),
      ?CODE(16#00, 16#20, 16#7A, "MIDI-hardware” R.Sowa"),
      ?CODE(16#00, 16#20, 16#7B, "Beyond Music Industrial Ltd"),
      ?CODE(16#00, 16#20, 16#7C, "Kiss Box B.V."),
      ?CODE(16#00, 16#20, 16#7D, "Misa Digital Technologies Ltd"),
      ?CODE(16#00, 16#20, 16#7E, "AI Musics Technology Inc"),
      ?CODE(16#00, 16#20, 16#7F, "Serato Inc LP"),
      ?CODE(16#00, 16#21, 16#00, "Limex"),
      ?CODE(16#00, 16#21, 16#01, "Kyodday (Tokai)"),
      ?CODE(16#00, 16#21, 16#02, "Mutable Instruments"),
      ?CODE(16#00, 16#21, 16#03, "PreSonus Software Ltd"),
      ?CODE(16#00, 16#21, 16#04, "Ingenico (was Xiring)"),
      ?CODE(16#00, 16#21, 16#05, "Fairlight Instruments Pty Ltd"),
      ?CODE(16#00, 16#21, 16#06, "Musicom Lab"),
      ?CODE(16#00, 16#21, 16#07, "Modal Electronics (Modulus/VacoLoco)"),
      ?CODE(16#00, 16#21, 16#08, "RWA (Hong Kong) Limited"),
      ?CODE(16#00, 16#21, 16#09, "Native Instruments"),
      ?CODE(16#00, 16#21, 16#0A, "Naonext"),
      ?CODE(16#00, 16#21, 16#0B, "MFB"),
      ?CODE(16#00, 16#21, 16#0C, "Teknel Research"),
      ?CODE(16#00, 16#21, 16#0D, "Ploytec GmbH"),
      ?CODE(16#00, 16#21, 16#0E, "Surfin Kangaroo Studio"),
      ?CODE(16#00, 16#21, 16#0F, "Philips Electronics HK Ltd"),
      ?CODE(16#00, 16#21, 16#10, "ROLI Ltd"),
      ?CODE(16#00, 16#21, 16#11, "Panda-Audio Ltd"),
      ?CODE(16#00, 16#21, 16#12, "BauM Software"),
      ?CODE(16#00, 16#21, 16#13, "Machinewerks Ltd."),
      ?CODE(16#00, 16#21, 16#14, "Xiamen Elane Electronics"),
      ?CODE(16#00, 16#21, 16#15, "Marshall Amplification PLC"),
      ?CODE(16#00, 16#21, 16#16, "Kiwitechnics Ltd"),
      ?CODE(16#00, 16#21, 16#17, "Rob Papen"),
      ?CODE(16#00, 16#21, 16#18, "Spicetone OU"),
      ?CODE(16#00, 16#21, 16#19, "V3Sound"),
      ?CODE(16#00, 16#21, 16#1A, "IK Multimedia"),
      ?CODE(16#00, 16#21, 16#1B, "Novalia Ltd"),
      ?CODE(16#00, 16#21, 16#1C, "Modor Music"),
      ?CODE(16#00, 16#21, 16#1D, "Ableton"),
      ?CODE(16#00, 16#21, 16#1E, "Dtronics"),
      ?CODE(16#00, 16#21, 16#1F, "ZAQ Audio"),
      ?CODE(16#00, 16#21, 16#20, "Muabaobao Education Technology Co Ltd"),
      ?CODE(16#00, 16#21, 16#21, "Flux Effects"),
      ?CODE(16#00, 16#21, 16#22, "Audiothingies (MCDA)"),
      ?CODE(16#00, 16#21, 16#23, "Retrokits"),
      ?CODE(16#00, 16#21, 16#24, "Morningstar FX Pte Ltd"),
      ?CODE(16#00, 16#21, 16#25, "Changsha Hotone Audio Co Ltd"),
      ?CODE(16#00, 16#21, 16#26, "Expressive E"),
      ?CODE(16#00, 16#21, 16#27, "Expert Sleepers Ltd"),
      ?CODE(16#00, 16#21, 16#28, "Timecode-Vision Technology"),
      ?CODE(16#00, 16#21, 16#29, "Hornberg Research GbR"),
      ?CODE(16#00, 16#21, 16#2A, "Sonic Potions"),
      ?CODE(16#00, 16#21, 16#2B, "Audiofront"),
      ?CODE(16#00, 16#21, 16#2C, "Fred’s Lab"),
      ?CODE(16#00, 16#21, 16#2D, "Audio Modeling"),
      ?CODE(16#00, 16#21, 16#2E, "C. Bechstein Digital GmbH"),
      ?CODE(16#00, 16#21, 16#2F, "Motas Electronics Ltd"),
      ?CODE(16#00, 16#21, 16#30, "Elk Audio"),
      ?CODE(16#00, 16#21, 16#31, "Sonic Academy Ltd"),
      ?CODE(16#00, 16#21, 16#32, "Bome Software"),
      ?CODE(16#00, 16#21, 16#33, "AODYO SAS"),
      ?CODE(16#00, 16#21, 16#34, "Pianoforce S.R.O"),
      ?CODE(16#00, 16#21, 16#35, "Dreadbox P.C."),
      ?CODE(16#00, 16#21, 16#36, "TouchKeys Instruments Ltd"),
      ?CODE(16#00, 16#21, 16#37, "The Gigrig Ltd"),
      ?CODE(16#00, 16#21, 16#38, "ALM Co"),
      ?CODE(16#00, 16#21, 16#39, "CH Sound Design"),
      ?CODE(16#00, 16#21, 16#3A, "Beat Bars"),
      ?CODE(16#00, 16#21, 16#3B, "Blokas"),
      ?CODE(16#00, 16#21, 16#3C, "GEWA Music GmbH"),
      ?CODE(16#00, 16#21, 16#3D, "dadamachines"),
      ?CODE(16#00, 16#21, 16#3E, "Augmented Instruments Ltd (Bela)"),
      ?CODE(16#00, 16#21, 16#3F, "Supercritical Ltd"),
      ?CODE(16#00, 16#21, 16#40, "Genki Instruments"),
      ?CODE(16#00, 16#21, 16#41, "Marienberg Devices Germany"),
      ?CODE(16#00, 16#21, 16#42, "Supperware Ltd"),
      ?CODE(16#00, 16#21, 16#43, "Imoxplus BVBA "),
      ?CODE(16#00, 16#21, 16#44, "Swapp Technologies SRL"),
      ?CODE(16#00, 16#21, 16#45, "Electra One S.R.O."),
      ?CODE(16#00, 16#21, 16#46, "Digital Clef Limited"),
      ?CODE(16#00, 16#21, 16#47, "Paul Whittington Group Ltd"),
      ?CODE(16#00, 16#21, 16#48, "Music Hackspace"),
      ?CODE(16#00, 16#21, 16#49, "Bitwig GMBH"),
      ?CODE(16#00, 16#21, 16#4A, "Enhancia"),
      ?CODE(16#00, 16#21, 16#4B, "KV 331"),
      ?CODE(16#00, 16#21, 16#4C, "Tehnicadelarte"),
      ?CODE(16#00, 16#21, 16#4D, "Endlesss Studio"),
      ?CODE(16#00, 16#21, 16#4E, "Dongguan MIDIPLUS Co., LTD"),
      ?CODE(16#00, 16#21, 16#4F, "Gracely Pty Ltd."),
      ?CODE(16#00, 16#21, 16#50, "Embodme"),
      ?CODE(16#00, 16#21, 16#51, "MuseScore"),
      ?CODE(16#00, 16#21, 16#52, "EPFL (E-Lab)"),
      ?CODE(16#00, 16#21, 16#53, "Orb3 Ltd."),
      ?CODE(16#00, 16#21, 16#54, "Pitch Innovations"),
      ?CODE(16#00, 16#21, 16#55, "Playces "),
      ?CODE(16#00, 16#21, 16#56, "UDO Audio LTD"),
      ?CODE(16#00, 16#21, 16#57, "RSS Sound Design"),
      ?CODE(16#00, 16#21, 16#58, "Nonlinear Labs GmbH"),
      ?CODE(16#00, 16#21, 16#59, "Robkoo Information & Technologies Co., Ltd."),
      ?CODE(16#00, 16#21, 16#5A, "Cari Electronic"),
      ?CODE(16#00, 16#21, 16#5B, "Oxi Electronic Instruments SL"),
      ?CODE(16#00, 16#21, 16#5C, "XMPT"),
      ?CODE(16#00, 16#21, 16#5D, "SHANGHAI HUAXIN MUSICAL INSTRUMENT "),
      ?CODE(16#00, 16#21, 16#5E, "Shenzhen Huashi Technology Co., Ltd"),
      ?CODE(16#00, 16#21, 16#60, "Guangzhou Rantion Technology Co., Ltd. "),
      ?CODE(16#00, 16#21, 16#61, "Ryme Music"),
      ?CODE(16#00, 16#21, 16#62, "GS Music"),
      ?CODE(16#00, 16#21, 16#63, "Shenzhen Flamma Innovation Co., Ltd"),
      ?CODE(16#00, 16#21, 16#64, "Shenzhen Mooer Audio Co.,LTD. "),
      ?CODE(16#00, 16#21, 16#65, "Raw Material Software Limited (JUCE)"),
      ?CODE(16#00, 16#21, 16#66, "Birdkids"),
      ?CODE(16#00, 16#21, 16#67, "Beijing QianYinHuLian Tech. Co"),
      ?CODE(16#00, 16#21, 16#68, "Nimikry Music OG"),
      ?CODE(16#00, 16#21, 16#69, "Newzik"),
      ?CODE(16#00, 16#21, 16#6A, "Hamburg Wave"),
      ?CODE(16#00, 16#21, 16#6B, "Grimm Audio"),
      ?CODE(16#00, 16#21, 16#6C, "Arcana Instruments LTD."),
      ?CODE(16#00, 16#21, 16#6D, "GameChanger Audio"),
      ?CODE(16#00, 16#21, 16#6E, "OakTone"),
      ?CODE(16#00, 16#21, 16#6F, "The Digi-Gurdy: A MIDI Hurdy Gurdy"),
      ?CODE(16#00, 16#21, 16#70, "MusiKraken"),
      ?CODE(16#00, 16#21, 16#71, "PhotoSynth > InterFACE"),
      ?CODE(16#00, 16#21, 16#72, "Instruments of Things"),
      ?CODE(16#00, 16#21, 16#73, "oodi"),
      ?CODE(16#00, 16#21, 16#74, "Komires Sp. z o.o."),
      ?CODE(16#00, 16#21, 16#75, "Lehle GmbH"),
      ?CODE(16#00, 16#21, 16#76, "Joué Music Instruments"),
      ?CODE(16#00, 16#21, 16#77, "Guangzhou Pearl River Amason Digital Musical Instrument Co. Ltd"),
      ?CODE(16#00, 16#21, 16#78, "Rhesus Engineering GmbH"),
      %% 40-5F reserved for Japanese Manufacturers
      ?CODE(16#40, "Kawai Musical Instruments MFG. CO. Ltd"),
      ?CODE(16#41, "Roland Corporation"),
      ?CODE(16#42, "Korg Inc."),
      ?CODE(16#43, "Yamaha Corporation"),
      ?CODE(16#44, "Casio Computer Co. Ltd"),
      ?CODE(16#46, "Kamiya Studio Co. Ltd"),
      ?CODE(16#47, "Akai Electric Co. Ltd."),
      ?CODE(16#48, "Victor Company of Japan, Ltd."),
      ?CODE(16#4B, "Fujitsu Limited"),
      ?CODE(16#4C, "Sony Corporation"),
      ?CODE(16#4E, "Teac Corporation"),
      ?CODE(16#50, "Matsushita Electric Industrial Co. , Ltd"),
      ?CODE(16#51, "Fostex Corporation"),
      ?CODE(16#52, "Zoom Corporation"),
      ?CODE(16#54, "Matsushita Communication Industrial Co., Ltd."),
      ?CODE(16#55, "Suzuki Musical Instruments MFG. Co., Ltd."),
      ?CODE(16#56, "Fuji Sound Corporation Ltd."),
      ?CODE(16#57, "Acoustic Technical Laboratory, Inc."),
      ?CODE(16#59, "Faith, Inc."),
      ?CODE(16#5A, "Internet Corporation"),
      ?CODE(16#5C, "Seekers Co. Ltd."),
      ?CODE(16#5F, "SD Card Association"),
      ?CODE(16#00, 16#40, 16#00, "Crimson Technology Inc."),
      ?CODE(16#00, 16#40, 16#01, "Softbank Mobile Corp"),
      ?CODE(16#00, 16#40, 16#03, "D&M Holdings Inc."),
      ?CODE(16#00, 16#40, 16#04, "Xing Inc."),
      ?CODE(16#00, 16#40, 16#05, "AlphaTheta Corporation"),
      ?CODE(16#00, 16#40, 16#06, "Pioneer Corporation"),
      ?CODE(16#00, 16#40, 16#07, "Slik Corporation")
     }.

