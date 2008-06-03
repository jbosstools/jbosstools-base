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
package org.jboss.tools.common.model.ui.widgets;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Layout;

import org.jboss.tools.common.model.ui.Insets;
import org.jboss.tools.common.model.ui.widgets.border.Border;

public class BorderedControl extends Canvas {
	private Border border;
	private static final int defaultStyle = SWT.NONE;

//	private BorderedControl(Composite parent, int style) {
//		super(parent, style);
//		this.setLayout(new BorderLayout());
//	}

	public BorderedControl(Composite parent, Border border) {
		this(parent, defaultStyle, border);
	}

	public BorderedControl(Composite parent, int style, Border border) {
		super(parent, style);
		this.border = border;
		this.setLayout(new BorderLayout(border.getBorderInsets()));
		this.setBackground(parent.getBackground());
		this.setForeground(parent.getForeground());
		this.setFont(parent.getFont());
		this.addPaintListener(border);
	}

	public void setBorder(Border border) {
		this.setRedraw(Boolean.FALSE.booleanValue());
		if ((border!=null)&&(this.border!=null)) this.removePaintListener(this.border);
		this.border = border;
		this.setLayout(new BorderLayout(border.getBorderInsets()));
		this.addPaintListener(border);
		this.setRedraw(Boolean.TRUE.booleanValue());
		this.redraw();
	}
	
	public Object getLayoutData () {
		Object layoutData = super.getLayoutData();
		Control[] children = getChildren();
		if (children.length > 0) {
			Control firstChildren = children[0];
			layoutData = firstChildren.getLayoutData();
		}
		return layoutData;
	}
	
	public void setLayoutData(Object layoutData) {
		super.setLayoutData(layoutData);
		Control[] children = getChildren();
		if (children.length > 0) {
			Control firstChildren = children[0];
			firstChildren.setLayoutData(layoutData);
		}
	}
	
	
	class BorderLayout extends Layout {
		private Insets margins;  
		
		private BorderLayout() {}

		public BorderLayout(Insets margins) {
			this.margins = margins;
		}

		public BorderLayout(int top, int left, int bottom, int right) {
			this.margins = new Insets(top, left, bottom, right);
		}

		public Point computeSize(Composite composite, int wHint, int hHint,	boolean force) {
			if (wHint != SWT.DEFAULT && hHint != SWT.DEFAULT) return new Point(wHint, hHint);

			Point result = null;
			Control[] children = composite.getChildren();
			if (children.length > 0) {
				Control firstChildren = children[0];
				result = new Point(0, 0);
				Point cp = firstChildren.computeSize(wHint, hHint, force);
				result.x = Math.max(result.x, cp.x);
				result.y = Math.max(result.y, cp.y);
			} else {
				Rectangle rect = composite.getClientArea();
				result = new Point(rect.width, rect.height);
			}
			result.x = result.x + margins.left + margins.right;
			result.y = result.y + margins.top + margins.bottom;

			result.x = Math.max(result.x, wHint);
			result.y = Math.max(result.y, hHint);;
			
			return result;
		}
		
		private Rectangle getClientArea(Composite c) {
			Rectangle rect = c.getClientArea();
			rect.x = rect.x + margins.left;
			rect.y = rect.y + margins.top;
			rect.width = rect.width - (margins.left + margins.right);
			rect.height = rect.height - (margins.top + margins.bottom);
			return rect;
		}
		
		public void layout(Composite composite, boolean force) {
			Rectangle rect = getClientArea(composite);
			Control[] children = composite.getChildren();
			children[0].setBounds(rect);
		}
	}
}
