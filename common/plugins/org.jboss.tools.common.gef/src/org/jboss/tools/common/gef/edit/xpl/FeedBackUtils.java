/*******************************************************************************
 * Copyright (c) 2000, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *     Exadel, Inc.
 *     Red Hat, Inc. 
 *******************************************************************************/
package org.jboss.tools.common.gef.edit.xpl;

import org.eclipse.draw2d.Polyline;
import org.eclipse.draw2d.geometry.Point;
import org.eclipse.draw2d.geometry.Rectangle;
import org.eclipse.draw2d.geometry.Transposer;
import org.eclipse.gef.EditPart;
import org.eclipse.gef.EditPolicy;
import org.eclipse.gef.GraphicalEditPart;
import org.eclipse.gef.Request;
import org.eclipse.gef.editpolicies.FlowLayoutEditPolicy;
import org.eclipse.gef.requests.DropRequest;

public class FeedBackUtils {

	/**
	 * Nobody canoot create instance of this class 
	 */
	private FeedBackUtils() {
		
	}
	
	public static void showLayoutTargetFeedBack(Request request,
			FlowLayoutEditPolicy policy, Polyline fb, int feedbackIndex,
			boolean isHorizontal) {
		if (policy.getHost().getChildren().size() == 0)
			return;
		Transposer transposer = new Transposer();
		transposer.setEnabled(!isHorizontal);

		boolean before = true;
		Rectangle r = null;
		if (feedbackIndex == -1) {
			before = false;
			feedbackIndex = policy.getHost().getChildren().size() - 1;
			EditPart editPart = (EditPart) policy.getHost().getChildren().get(
					feedbackIndex);
			r = transposer.t(getAbsoluteBounds((GraphicalEditPart) editPart));
		} else {
			EditPart editPart = (EditPart) policy.getHost().getChildren().get(
					feedbackIndex);
			r = transposer.t(getAbsoluteBounds((GraphicalEditPart) editPart));
			Point p = transposer.t(((DropRequest) request).getLocation());
			if (p.x <= r.x + (r.width / 2))
				before = true;
			else {
				before = false;
				feedbackIndex--;
				editPart = (EditPart) policy.getHost().getChildren().get(
						feedbackIndex);
				r = transposer
						.t(getAbsoluteBounds((GraphicalEditPart) editPart));
			}
		}
		int x = Integer.MIN_VALUE;
		if (before) {
			if (feedbackIndex > 0) {
				// Need to determine if a line break.
				Rectangle boxPrev = transposer
						.t(getAbsoluteBounds((GraphicalEditPart) policy
								.getHost().getChildren().get(feedbackIndex - 1)));
				int prevRight = boxPrev.right();
				if (prevRight < r.x) {
					// Not a line break
					x = prevRight + (r.x - prevRight) / 2;
				} else if (prevRight == r.x) {
					x = prevRight + 1;
				}
			}
			if (x == Integer.MIN_VALUE) {
				// It is a line break.
				Rectangle parentBox = transposer
						.t(getAbsoluteBounds((GraphicalEditPart) policy
								.getHost()));
				x = r.x - 5;
				if (x < parentBox.x)
					x = parentBox.x + (r.x - parentBox.x) / 2;
			}
		} else {
			Rectangle parentBox = transposer
					.t(getAbsoluteBounds((GraphicalEditPart) policy.getHost()));
			int rRight = r.x + r.width;
			int pRight = parentBox.x + parentBox.width;
			x = rRight + 5;
			if (x > pRight)
				x = rRight + (pRight - rRight) / 2;
		}
		Point p1 = new Point(x, r.y - 4);
		p1 = transposer.t(p1);
		fb.translateToRelative(p1);
		Point p2 = new Point(x, r.y + r.height + 4);
		p2 = transposer.t(p2);
		fb.translateToRelative(p2);
		fb.setPoint(p1, 0);
		fb.setPoint(p2, 1);

	}

	public static Rectangle getAbsoluteBounds(GraphicalEditPart ep) {
		Rectangle bounds = ep.getFigure().getBounds().getCopy();
		ep.getFigure().translateToAbsolute(bounds);
		return bounds;
	}

}
