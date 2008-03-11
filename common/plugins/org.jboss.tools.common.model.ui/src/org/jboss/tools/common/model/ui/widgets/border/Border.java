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

import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.widgets.Composite;

import org.jboss.tools.common.model.ui.Insets;

public abstract class Border implements PaintListener {
	
	// Constructors
	public Border() {}
	
	// abstract	
	public abstract Insets getBorderInsets();
	public abstract boolean isBorderOpaque();
	public abstract void paintBorder(Composite composite, GC gc);

	// implements PaintListener
	public void paintControl(PaintEvent event) {
		Composite composite = (Composite) event.widget; // it instanse of Border class
		GC gc = event.gc;
		paintBorder(composite, gc);	 
	}
}
