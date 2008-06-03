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
		put("Text.Style", SWT.NONE);
		put("Text.Border", textBorder);

		// Note
		put("Note.Style", SWT.NONE | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		put("Note.Border", textBorder);
		put("Note.ValidateStrategy", 1);

		// Combo
		put("Combo.Style", SWT.FLAT);
		put("Combo.Border", comboBorder);

		// Button
		put("Button.Style", SWT.FLAT);

		// CheckBox
		put("CheckBox.Style", SWT.FLAT | SWT.CHECK | SWT.LEFT);

		// List
		put("List.Style", SWT.NONE);
		put("List.Border", listBorder);

		// Tree
		put("Tree.Style", SWT.NONE);
		put("Tree.Border", listBorder);

		// Table
		put("Table.Style", SWT.NONE);
		put("Table.Border", listBorder);
		
		// Separator
		put("Separator.Background", separatorColor);
		
		put("Label.Foreground", hyperlinkColor);
	}

	public FormToolkit getToolkit(Display display) {
		if(toolkit == null) {
			toolkit = new FormToolkit(display);
		}
		return toolkit;
	}
	
	

}
