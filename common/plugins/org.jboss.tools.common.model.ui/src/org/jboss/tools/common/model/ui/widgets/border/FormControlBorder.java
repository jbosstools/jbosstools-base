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
package org.jboss.tools.common.model.ui.widgets.border;

import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.Insets;

public class FormControlBorder extends Border {
	private Color inner;
	private Color outer; 
	private Insets insets;
	
	public FormControlBorder(Insets insets) {
		this.insets = insets;
	}
	
	public FormControlBorder(Color outer, Color inner, Insets insets) {
		this.inner = inner;
		this.outer = outer;
		this.insets = insets;
	}
	public FormControlBorder(Color outer, Color inner) {
		this.inner = inner;
		this.outer = outer;
		this.insets = new Insets(2,2,2,2);
	}

	public Insets getBorderInsets() {
		return insets;
	}

	public boolean isBorderOpaque() {
		return false;
	}

	public void paintBorder(Composite composite, GC gc) {
		Rectangle r = composite.getClientArea();
		if(outer != null) gc.setForeground(outer);
		gc.drawRectangle(r.x, r.y, r.width-1, r.height-1);
		if(inner != null) gc.setForeground(inner);
		gc.drawRectangle(r.x+1, r.y+1, r.width-3, r.height-3);
		
	}
}
