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
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.forms.widgets.FormToolkit;

import org.jboss.tools.common.model.ui.Insets;
import org.jboss.tools.common.model.ui.widgets.border.Border;
import org.jboss.tools.common.model.ui.widgets.border.FormControlBorder;

public class WhiteSettings extends DefaultSettings {
	
	private Color listBorderColor;
	private Color separatorColor;
	
	private Border textBorder;
	private Border comboBorder;
	private Border listBorder;
	
	private FormToolkit toolkit = null;
	
	public WhiteSettings() {
		super();
	}

	protected void initBorders() {
		super.initBorders();
		textBorder = new FormControlBorder(defaultForeground, defaultBackground, new Insets(3,2,4,2));
		comboBorder = new FormControlBorder(defaultForeground, defaultBackground);
		listBorder = new FormControlBorder(listBorderColor, defaultBackground);
	}

	protected void initColors() {
		super.initColors();
		defaultBackground = new Color(Display.getCurrent(), 255, 255, 255); //Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
		listBorderColor = new Color(Display.getCurrent(), 195, 191, 179);
		separatorColor = new Color(Display.getCurrent(), 152, 170, 203);
	}

	protected void initCursors() {
		super.initCursors();
		handCursor = new Cursor(Display.getCurrent(), SWT.CURSOR_HAND);
	}

	protected void initFonts() {
		super.initFonts();
	}

	protected void initSettings() {
		super.initSettings();
		// Text
		put("Text.Style", SWT.NONE); //$NON-NLS-1$
		put("Text.Border", textBorder); //$NON-NLS-1$

		// Note
		put("Note.Style", SWT.NONE | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL); //$NON-NLS-1$
		put("Note.Border", textBorder); //$NON-NLS-1$
		put("Note.ValidateStrategy", 1); //$NON-NLS-1$

		// Combo
		put("Combo.Style", SWT.FLAT); //$NON-NLS-1$
		put("Combo.Border", comboBorder); //$NON-NLS-1$

		// Button
		put("Button.Style", SWT.FLAT); //$NON-NLS-1$

		// CheckBox
		put("CheckBox.Style", SWT.FLAT | SWT.CHECK | SWT.LEFT); //$NON-NLS-1$

		// List
		put("List.Style", SWT.NONE); //$NON-NLS-1$
		put("List.Border", listBorder); //$NON-NLS-1$

		// Tree
		put("Tree.Style", SWT.NONE); //$NON-NLS-1$
		put("Tree.Border", listBorder); //$NON-NLS-1$

		// Table
		put("Table.Style", SWT.NONE); //$NON-NLS-1$
		put("Table.Border", listBorder); //$NON-NLS-1$
		
		// Separator
		put("Separator.Background", separatorColor); //$NON-NLS-1$
		
		put("Label.Foreground", hyperlinkColor); //$NON-NLS-1$
	}

	public FormToolkit getToolkit(Display display) {
		if(toolkit == null) {
			toolkit = new FormToolkit(display);
		}
		return toolkit;
	}
	
	

}
