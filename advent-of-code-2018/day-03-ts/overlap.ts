interface Segment1D {
	x1: number;
	x2: number;
}

export const overlap1D = (s1: Segment1D, s2: Segment1D): Segment1D | undefined => {
	const [seg1, seg2] = (s1.x1 <= s2.x1) ? [s1, s2] : [s2, s1];

	if (seg1.x2 >= seg2.x2) {
		// seg1 contains seg2
		return seg2;
	} else if (seg1.x2 >= seg2.x1) {
		return { x1: seg2.x1, x2: seg1.x2 };
	}

	return;
};

export interface Rectangle {
	x: number;
	y: number;
	w: number;
	h: number;
}

export const overlapArea = (r1: Rectangle, r2: Rectangle) => {
	const x = overlap1D(
		{ x1: r1.x, x2: r1.x + r1.w - 1 },
		{ x1: r2.x, x2: r2.x + r2.w - 1 }
	);

	if (!x) return;

	const y = overlap1D(
		{ x1: r1.y, x2: r1.y + r1.h - 1 },
		{ x1: r2.y, x2: r2.y + r2.h - 1 }
	);

	if (!y) return;

	return {
		x: x.x1,
		y: y.x1,
		w: x.x2 - x.x1 + 2,
		h: y.x2 - y.x1 + 2,
	}
}