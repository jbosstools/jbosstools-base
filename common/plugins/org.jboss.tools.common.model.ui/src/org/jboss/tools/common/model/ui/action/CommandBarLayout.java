/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.model.ui.action;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.*;
import org.eclipse.swt.widgets.*;
import org.eclipse.swt.graphics.*;

public class CommandBarLayout extends Layout {
	private static final int DEFAULT_GAP = 5;
	private static final int DEFAULT_LEFT = 5;
	private static final int DEFAULT_RIGHT = 5;
	private static final int DEFAULT_TOP = 5;
	private static final int DEFAULT_BOTTOM = 5;
	
	
	public int buttonWidth = SWT.DEFAULT;
	public int buttonHeight = SWT.DEFAULT;
	public int direction = SWT.HORIZONTAL; ///SWT.VERTICAL;
	public int gap = SWT.DEFAULT;
	public int top = SWT.DEFAULT, left = SWT.DEFAULT, bottom = SWT.DEFAULT, right = SWT.DEFAULT;
	public boolean iconsOnly = false;
	public boolean asToolBar = false;
	public int alignment = SWT.LEFT; /// SWT.RIGHT
	
	public void setMargins(int top, int left, int bottom, int right) {
		this.top = top;
		this.left = left;
		this.bottom = bottom;
		this.right = right;
	}
	
	protected Point computeSize(Composite composite, int wHint, int hHint, boolean flushCache) {
		int w = left() + right();
		int h = top() + bottom();
		Control[] cs = composite.getChildren();
		if(cs == null || cs.length == 0) {
			int width = left() + right() + ((buttonWidth != SWT.DEFAULT)?buttonWidth:0);
			int height = top() + bottom() + ((buttonHeight != SWT.DEFAULT)?buttonHeight:0);
			return new Point(width,height);
		}
		int bw = computeButtonWidth(cs);
		int bh = computeButtonHeight(cs);
		if(direction == SWT.HORIZONTAL) {
			w = w + (cs.length - 1) * gap() + cs.length * bw;
			h = h + bh;
		} else {
			w = w + bw;
			h = h + (cs.length - 1) * gap() + cs.length * bh;
		}
		return new Point(w, h);
	}
	
	protected void layout(Composite composite, boolean flushCache) {
		Point size = composite.getSize();
		Point s = computeSize(composite, SWT.DEFAULT, SWT.DEFAULT, false);
		Control[] cs = composite.getChildren();
		int bw = computeButtonWidth(cs);
		int bh = computeButtonHeight(cs);
		int x = left(), y = top(), dx = gap() + bw, dy = gap() + bh;
		if(alignment == SWT.RIGHT) {
			x = size.x - s.x + left();
		}
		if(direction == SWT.HORIZONTAL) {
			for (int i = 0; i < cs.length; i++) {
				cs[i].setBounds(x, y, bw, bh);
				x += dx;
			}
		} else {
			for (int i = 0; i < cs.length; i++) {
				cs[i].setBounds(x, y, bw, bh);
				y += dy;
			}
		}	
	}
	
	private int computeButtonHeight(Control[] cs) {
		if ((buttonHeight != SWT.DEFAULT)&&(!iconsOnly)) return buttonHeight; // ignore custom button height
		int height = 0;
		for (int i = 0; i < cs.length; i++) {
			int h = cs[i].computeSize(SWT.DEFAULT, SWT.DEFAULT).y;
			if(iconsOnly) h -= 6;
			height = Math.max(h, height);
		}
		return height;
	}
	private int computeButtonWidth(Control[] cs) {
		if ((buttonWidth != SWT.DEFAULT)&&(!iconsOnly)) return buttonWidth; // ignore custom button width			
		int width = 0;
		for (int i = 0; i < cs.length; i++) {
			int w = cs[i].computeSize(SWT.DEFAULT, SWT.DEFAULT).x;
			if(iconsOnly) w -= 6;
			width = Math.max(w, width);
			if(!iconsOnly) {
				int w1 = convertHorizontalDLUsToPixels(cs[i], IDialogConstants.BUTTON_WIDTH);
				width = Math.max(w1, width);
			}
		}
		return width;
	}

	protected int convertHorizontalDLUsToPixels(Control control, int dlus) {
		GC gc= new GC(control);
		gc.setFont(control.getFont());
		int averageWidth= gc.getFontMetrics().getAverageCharWidth();
		gc.dispose();
	
		double horizontalDialogUnitSize = averageWidth * 0.25;
	
		return (int)Math.round(dlus * horizontalDialogUnitSize);
	}
	

	private int top() {
		return (top != SWT.DEFAULT) ? top : DEFAULT_TOP;
	}

	private int left() {
		return (left != SWT.DEFAULT) ? left : DEFAULT_LEFT;
	}
	
	private int bottom() {
		return (bottom != SWT.DEFAULT) ? bottom : DEFAULT_BOTTOM;
	}

	private int right() {
		return (right != SWT.DEFAULT) ? right : DEFAULT_RIGHT;
	}

	private int gap() {
		return (gap != SWT.DEFAULT) ? gap : DEFAULT_GAP;
	}
}
