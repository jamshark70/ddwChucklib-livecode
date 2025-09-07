/**
    Chucklib-livecode: A framework for live-coding improvisation of electronic music
    Copyright (C) 2018  Henry James Harkins

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
**/

// beatsPerBar, *and* mappings of # of subdivisions to durations
// have a separate ClMeter for each beatsPerBar

Proto {
	~beatsPerBar = 4;
	~divs = [
		[4],
		[2, 2],
		[1, 1, 1, 1],
		0.5 ! 8
	];
	~prep = {
		~divisions = IdentityDictionary.new;
		if(~divs.size > 0) {
			~divs.do { |row|
				if(~divisions[row.size].isNil) {
					~divisions[row.size] = row
				} {
					Error("ClMeter: Multiple subdivisions for the same size").throw;
				}
			};
		} {
			Error("Did not provide subdivisions to ClMeter").throw;
		};
		currentEnvironment
	};
	~get = { |in|
		if(in.size == 0) {
			~divisions[in]
		} {
			~divisions[in.size]
		}
	};
} => PR(\clMeter);

Proto {
	~prep = {
		~all = Dictionary.new;
		~all.put(4, PR(\clMeter).copy.prep);
		currentEnvironment
	};
	~add = { |rows|
		var sums = rows.collect(_.sum);
		var new;
		if(sums.differentiate.drop(1).any(_ != 0)) {
			Error("Meter rows should all have the same sum, sums are " ++ sums).throw;
		};
		new = PR(\clMeter).copy.putAll((
			beatsPerBar: sums[0],
			divs: rows
		)).prep;
		~all.put(sums[0], new);
		new
	};
	~get = { |bpb, in|
		var thing = ~all[bpb];
		if(thing.isNil) {
			nil
		} {
			thing.get(in)
		}
	};
} => PR(\clMeterRepo);

PR(\clMeterRepo) => BP(\clMeters);
