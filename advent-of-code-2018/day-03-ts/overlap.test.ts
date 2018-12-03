import { assert } from 'chai';
import * as fc from 'fast-check';
import { overlap1D } from './overlap';

describe('overlap1D', () => {
	it('is commutative', () => {
		fc.assert(
			fc.property(fc.nat(), fc.nat(), fc.nat(), fc.nat(), (seg1x1, seg1x2, seg2x1, seg2x2) => {
				fc.pre(seg1x1 <= seg1x2);
				fc.pre(seg2x1 <= seg2x2);
				const seg1 = { x1: seg1x1, x2: seg1x2 };
				const seg2 = { x1: seg2x1, x2: seg2x2 };
				const ov1 = overlap1D(seg1, seg2);
				const ov2 = overlap1D(seg2, seg1);
				fc.pre(ov1 !== undefined);

				assert.deepEqual(ov1!, ov2!);
			})
		);
	});

	it('finds overlaps when the segments are equal', () => {
		fc.property(fc.nat(), fc.nat(), (x1, x2) => {
			fc.pre(x1 <= x2);
			const seg = { x1, x2 };

			assert.deepEqual(overlap1D(seg, seg), seg);
		})
	});

	it('finds overlaps', () => {
		assert.deepEqual(
			overlap1D({ x1: 1, x2: 4 }, { x1: 3, x2: 5 }),
			{ x1: 3, x2: 4 }
		)
	})
});