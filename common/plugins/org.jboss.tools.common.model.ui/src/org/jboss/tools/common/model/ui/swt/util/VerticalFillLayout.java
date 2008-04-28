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
package org.jboss.tools.common.model.ui.swt.util;

import org.eclipse.swt.*;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.*;

public class VerticalFillLayout extends Layout {
	public int leftMargin = SWT.DEFAULT;
	public int rightMargin = SWT.DEFAULT;
	
	public static Control createSeparator(Composite parent, int height) {
		Composite c = new Composite(parent, SWT.NONE);
		VerticalFillLayoutData d = new VerticalFillLayoutData();
		d.heightHint = height;
		c.setLayoutData(d);
		return c;
	}

	protected Point computeSize(Composite composite, int wHint, int hHint, boolean flushCache) {
		int w = 0;
		int m = 0;
		if(leftMargin != SWT.DEFAULT) m = leftMargin;
		if(rightMargin != SWT.DEFAULT) m += rightMargin;
		int h = 0;
		Control[] cs = composite.getChildren();
		for (int i = 0; i < cs.length; i++) {
//			if(cs[i].isDisposed() || !cs[i].isVisible()) continue;
			if(!cs[i].isVisible() && !cs[i].isEnabled()) continue;
			Point p = cs[i].computeSize(wHint, SWT.DEFAULT);
			Object d = cs[i].getLayoutData();
			VerticalFillLayoutData data = (d instanceof VerticalFillLayoutData) ? (VerticalFillLayoutData)d : null;
			if(data != null && data.heightHint != SWT.DEFAULT) {
				h += data.heightHint;
			} else { 
				h += p.y;
			}
			if(w < p.x) w = p.x;    	
		}
		return new Point(w + m, h);
	}
	
	protected void layout(Composite composite, boolean flushCache) {
		int x = composite.getClientArea().x;
		int y = composite.getClientArea().y;
		int width = composite.getClientArea().width;
//		int height = composite.getClientArea().height;
		if(leftMargin != SWT.DEFAULT) {
			x += leftMargin;
			width -= leftMargin;
		} 
		if(rightMargin != SWT.DEFAULT) {
			width -= rightMargin;
		} 

		Control[] cs = composite.getChildren();
		for (int i = 0; i < cs.length; i++) {
//			if(cs[i].isDisposed() || !cs[i].isVisible()) continue;
			if(!cs[i].isVisible() && !cs[i].isEnabled()) continue;
			Point p = cs[i].computeSize(width, SWT.DEFAULT);
			Object d = cs[i].getLayoutData();
			VerticalFillLayoutData data = (d instanceof VerticalFillLayoutData) ? (VerticalFillLayoutData)d : null;
			int dh = (data != null && data.heightHint != SWT.DEFAULT)
					? data.heightHint : p.y;  
			cs[i].setBounds(x, y, width, dh);
			y += dh;
		}		
	}	

}
