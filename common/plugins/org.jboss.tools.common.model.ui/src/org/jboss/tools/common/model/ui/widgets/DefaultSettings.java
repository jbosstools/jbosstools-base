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

import java.util.HashMap;

import org.eclipse.jface.resource.JFaceColors;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CCombo;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.forms.widgets.FormToolkit;

import org.jboss.tools.common.model.ui.Insets;
import org.jboss.tools.common.model.ui.widgets.border.Border;
import org.jboss.tools.common.model.ui.widgets.border.FormControlBorder;

public class DefaultSettings implements IWidgetSettings {
	// settings
	private HashMap<String,Object> settings = new HashMap<String,Object>();

	// static access	
	private static IWidgetSettings defaultSettings;
	
	// colors
	protected Color redBackground;
	protected Color whiteBackground;
	protected Color defaultBackground;
	protected Color defaultForeground;
	protected Color hyperlinkColor;
	protected Color activeHyperlinkColor;
	
	// fonts
	protected Font defaultFont;
	protected Font headerFont;
	protected Font titleFont;
	
	// cursors
	protected Cursor defaultCursor;
	protected Cursor handCursor;
	
	// borders
	protected Border defaultBorder;
	
	public static IWidgetSettings getDefault() {
		if (defaultSettings==null) defaultSettings = new DefaultSettings();
		return defaultSettings;
	}
	
	protected void initColors() {
		defaultBackground = Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_BACKGROUND);
		defaultForeground = Display.getCurrent().getSystemColor(SWT.COLOR_WIDGET_FOREGROUND);
		whiteBackground = Display.getCurrent().getSystemColor(SWT.COLOR_WHITE);
		redBackground = Display.getCurrent().getSystemColor(SWT.COLOR_RED);
		hyperlinkColor = JFaceColors.getHyperlinkText(Display.getCurrent());
		activeHyperlinkColor = JFaceColors.getActiveHyperlinkText(Display.getCurrent());
	}
	
	protected void initFonts() {
		defaultFont = JFaceResources.getDefaultFont();
		headerFont = JFaceResources.getBannerFont();
		titleFont = JFaceResources.getHeaderFont();
	}
	
	protected void initCursors() {
		defaultCursor = new Cursor(Display.getCurrent(), SWT.CURSOR_ARROW);
		handCursor = new Cursor(Display.getCurrent(), SWT.CURSOR_HAND);
	}
	
	protected void initBorders() {
		defaultBorder = new FormControlBorder(new Insets(0,0,0,0));
	}
	
	protected void initSettings() {
		// Unknow control
		put("Control.Style", SWT.NONE);
		put("Control.Background", defaultBackground);
		put("Control.Foreground", defaultForeground);
		put("Control.Font", defaultFont);
		
		// Composite
		put("Composite.Style", SWT.NONE);
		put("Composite.Background", defaultBackground);
		put("Composite.Foreground", defaultForeground);
		put("Composite.Font", defaultFont);
		
		// Label
		put("Label.Style", SWT.NONE);
				// Please, do not put whiteBackground here, it spoils all wizards
		put("Label.Background", defaultBackground);
		put("Label.Foreground", defaultForeground);
		put("Label.Font", defaultFont);
		put("Label.Font.Title", titleFont);
		put("Label.Font.Header", headerFont);

		// Hyperlink
		put("Hyperlink.Style", SWT.NONE);
		put("Hyperlink.Background", defaultBackground);
		put("Hyperlink.Foreground", hyperlinkColor);
		put("Hyperlink.ActiveColor", activeHyperlinkColor);
		put("Hyperlink.Font", defaultFont);
		put("Hyperlink.ActiveCursor", handCursor);
		put("Hyperlink.Disabled", defaultForeground);

		// Text
		put("Text.Style", SWT.BORDER);
		put("Text.Background", whiteBackground);
		put("Text.Foreground", defaultForeground);
		put("Text.Font", defaultFont);

		// Note
		put("Note.Style", SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		put("Note.Background", whiteBackground);
		put("Note.Foreground", defaultForeground);
		put("Note.Font", defaultFont);
		put("Note.ValidateStrategy", 0);

		// Combo
		put("Combo.Style", SWT.BORDER);
		put("Combo.Background", whiteBackground);
		put("Combo.Background.Disabled", defaultBackground);
		put("Combo.Foreground", defaultForeground);
		put("Combo.Font", defaultFont);

		// Button
		put("Button.Style", SWT.PUSH);
		put("Button.Background", defaultBackground);
		put("Button.Foreground", defaultForeground);
		put("Button.Font", defaultFont);

		// CheckBox
		put("CheckBox.Style", SWT.CHECK);
		put("CheckBox.Background", defaultBackground);
		put("CheckBox.Foreground", defaultForeground);
		put("CheckBox.Font", defaultFont);

		// List
		put("List.Style", SWT.BORDER);
		put("List.Background", whiteBackground);
		put("List.Foreground", defaultForeground);
		put("List.Font", defaultFont);

		// Tree
		put("Tree.Style", SWT.BORDER);
		put("Tree.Background", whiteBackground);
		put("Tree.Foreground", defaultForeground);
		put("Tree.Font", defaultFont);

		// Table
		put("Table.Style", SWT.BORDER);
		put("Table.Background", whiteBackground);
		put("Table.Foreground", defaultForeground);
		put("Table.Font", defaultFont);
	}
	
	public DefaultSettings() {
		initColors();
		initFonts();
		initCursors();
		initBorders();
		initSettings();
	}

	public Object getObject(String key) {
		return settings.get(key);
	}

	public int getInt(String key) {
		Integer integer = (Integer)settings.get(key);
		if (integer!=null) return integer.intValue(); 
		return SWT.DEFAULT;
	}

	public int getStyle(String key) {
		Integer integer = (Integer)settings.get(key);
		if (integer!=null) return integer.intValue(); 
		return SWT.NONE;
	}
	public Color getColor(String key) {
		Color color = (Color)settings.get(key);
		if (color==null) color = redBackground; 
		return color;
	}
	public Font getFont(String key) {
		Font font = (Font)settings.get(key);
		if (font==null) font = defaultFont;
		return font;
	}
	public Border getBorder(String key) {
		Border border = (Border)settings.get(key);
		if (border == null) border = defaultBorder;
		return border;
	}
	public Cursor getCursor(String key) {
		Cursor cursor = (Cursor)settings.get(key);
		if (cursor == null) cursor = defaultCursor;
		return cursor;
	}
		
	protected void put(String key, Object object) {
		settings.put(key, object);
	}
	protected void put(String key, int i) {
		settings.put(key, new Integer(i));
	}
	protected void put(String key, Color color) {
		settings.put(key, color);
	}
	protected void put(String key, Font font) {
		settings.put(key, font);
	}
	protected void put(String key, Border border) {
		settings.put(key, border);
	}
	protected void put(String key, Cursor cursor) {
		settings.put(key, cursor);
	}

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.model.ui.widgets.IWidgetSettings#setupControl(org.eclipse.swt.widgets.Control)
	 */
	public void setupControl(Control control) {
		FormToolkit toolkit = getToolkit(control.getDisplay());
		if(toolkit != null && control instanceof Composite) {
			toolkit.adapt((Composite)control);
		}
		if (control instanceof CCombo || control instanceof Combo) {
			control.setBackground(getColor("Combo.Background"));
			control.setForeground(getColor("Combo.Foreground"));
			control.setFont(getFont("Combo.Font"));
		} else if (control instanceof Label) {
			control.setBackground(getColor("Label.Background"));
			control.setForeground(getColor("Label.Foreground"));
			control.setFont(getFont("Label.Font"));
		} else if (control instanceof Text) {
			control.setBackground(getColor("Text.Background"));
			control.setForeground(getColor("Text.Foreground"));
			control.setFont(getFont("Text.Font"));
		} else if (control instanceof Button) {
			control.setBackground(getColor("Button.Background"));
			control.setForeground(getColor("Button.Foreground"));
			control.setFont(getFont("Button.Font"));
		} else if (control instanceof List) {
			control.setBackground(getColor("List.Background"));
			control.setForeground(getColor("List.Foreground"));
			control.setFont(getFont("List.Font"));
		} else if (control instanceof Tree) {
			control.setBackground(getColor("Tree.Background"));
			control.setForeground(getColor("Tree.Foreground"));
			control.setFont(getFont("Tree.Font"));
		} else if (control instanceof Table) {
			control.setBackground(getColor("Table.Background"));
			control.setForeground(getColor("Table.Foreground"));
			control.setFont(getFont("Table.Font"));
		} else if (control instanceof Composite) {
			control.setBackground(getColor("Composite.Background"));
			control.setForeground(getColor("Composite.Foreground"));
			control.setFont(getFont("Composite.Font"));
		} else { // unknow control
			control.setBackground(getColor("Control.Background"));
			control.setForeground(getColor("Control.Foreground"));
			control.setFont(getFont("Control.Font"));
		}
	}
	
	public FormToolkit getToolkit(Display display) {
		return null;
	}

}
