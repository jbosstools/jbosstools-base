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
		put("Control.Style", SWT.NONE); //$NON-NLS-1$
		put("Control.Background", defaultBackground); //$NON-NLS-1$
		put("Control.Foreground", defaultForeground); //$NON-NLS-1$
		put("Control.Font", defaultFont); //$NON-NLS-1$
		
		// Composite
		put("Composite.Style", SWT.NONE); //$NON-NLS-1$
		put("Composite.Background", defaultBackground); //$NON-NLS-1$
		put("Composite.Foreground", defaultForeground); //$NON-NLS-1$
		put("Composite.Font", defaultFont); //$NON-NLS-1$
		
		// Label
		put("Label.Style", SWT.NONE); //$NON-NLS-1$
				// Please, do not put whiteBackground here, it spoils all wizards
		put("Label.Background", defaultBackground); //$NON-NLS-1$
		put("Label.Foreground", defaultForeground); //$NON-NLS-1$
		put("Label.Font", defaultFont); //$NON-NLS-1$
		put("Label.Font.Title", titleFont); //$NON-NLS-1$
		put("Label.Font.Header", headerFont); //$NON-NLS-1$

		// Hyperlink
		put("Hyperlink.Style", SWT.NONE); //$NON-NLS-1$
		put("Hyperlink.Background", defaultBackground); //$NON-NLS-1$
		put("Hyperlink.Foreground", hyperlinkColor); //$NON-NLS-1$
		put("Hyperlink.ActiveColor", activeHyperlinkColor); //$NON-NLS-1$
		put("Hyperlink.Font", defaultFont); //$NON-NLS-1$
		put("Hyperlink.ActiveCursor", handCursor); //$NON-NLS-1$
		put("Hyperlink.Disabled", defaultForeground); //$NON-NLS-1$

		// Text
		put("Text.Style", SWT.BORDER); //$NON-NLS-1$
		put("Text.Background", whiteBackground); //$NON-NLS-1$
		put("Text.Foreground", defaultForeground); //$NON-NLS-1$
		put("Text.Font", defaultFont); //$NON-NLS-1$

		// Note
		put("Note.Style", SWT.BORDER | SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL); //$NON-NLS-1$
		put("Note.Background", whiteBackground); //$NON-NLS-1$
		put("Note.Foreground", defaultForeground); //$NON-NLS-1$
		put("Note.Font", defaultFont); //$NON-NLS-1$
		put("Note.ValidateStrategy", 0); //$NON-NLS-1$

		// Combo
		put("Combo.Style", SWT.BORDER); //$NON-NLS-1$
		put("Combo.Background", whiteBackground); //$NON-NLS-1$
		put("Combo.Background.Disabled", defaultBackground); //$NON-NLS-1$
		put("Combo.Foreground", defaultForeground); //$NON-NLS-1$
		put("Combo.Font", defaultFont); //$NON-NLS-1$

		// Button
		put("Button.Style", SWT.PUSH); //$NON-NLS-1$
		put("Button.Background", defaultBackground); //$NON-NLS-1$
		put("Button.Foreground", defaultForeground); //$NON-NLS-1$
		put("Button.Font", defaultFont); //$NON-NLS-1$

		// CheckBox
		put("CheckBox.Style", SWT.CHECK); //$NON-NLS-1$
		put("CheckBox.Background", defaultBackground); //$NON-NLS-1$
		put("CheckBox.Foreground", defaultForeground); //$NON-NLS-1$
		put("CheckBox.Font", defaultFont); //$NON-NLS-1$

		// List
		put("List.Style", SWT.BORDER); //$NON-NLS-1$
		put("List.Background", whiteBackground); //$NON-NLS-1$
		put("List.Foreground", defaultForeground); //$NON-NLS-1$
		put("List.Font", defaultFont); //$NON-NLS-1$

		// Tree
		put("Tree.Style", SWT.BORDER); //$NON-NLS-1$
		put("Tree.Background", whiteBackground); //$NON-NLS-1$
		put("Tree.Foreground", defaultForeground); //$NON-NLS-1$
		put("Tree.Font", defaultFont); //$NON-NLS-1$

		// Table
		put("Table.Style", SWT.BORDER); //$NON-NLS-1$
		put("Table.Background", whiteBackground); //$NON-NLS-1$
		put("Table.Foreground", defaultForeground); //$NON-NLS-1$
		put("Table.Font", defaultFont); //$NON-NLS-1$
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
		settings.put(key, Integer.valueOf(i));
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
		if(control instanceof Composite) {
			toolkit.adapt((Composite)control);
		}
		if (control instanceof CCombo || control instanceof Combo) {
			control.setBackground(getColor("Combo.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Combo.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Combo.Font")); //$NON-NLS-1$
		} else if (control instanceof Label) {
			control.setBackground(getColor("Label.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Label.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Label.Font")); //$NON-NLS-1$
		} else if (control instanceof Text) {
			control.setBackground(getColor("Text.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Text.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Text.Font")); //$NON-NLS-1$
		} else if (control instanceof Button) {
			control.setBackground(getColor("Button.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Button.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Button.Font")); //$NON-NLS-1$
		} else if (control instanceof List) {
			control.setBackground(getColor("List.Background")); //$NON-NLS-1$
			control.setForeground(getColor("List.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("List.Font")); //$NON-NLS-1$
		} else if (control instanceof Tree) {
			control.setBackground(getColor("Tree.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Tree.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Tree.Font")); //$NON-NLS-1$
		} else if (control instanceof Table) {
			control.setBackground(getColor("Table.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Table.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Table.Font")); //$NON-NLS-1$
		} else if (control instanceof Composite) {
			control.setBackground(getColor("Composite.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Composite.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Composite.Font")); //$NON-NLS-1$
		} else { // unknow control
			control.setBackground(getColor("Control.Background")); //$NON-NLS-1$
			control.setForeground(getColor("Control.Foreground")); //$NON-NLS-1$
			control.setFont(getFont("Control.Font")); //$NON-NLS-1$
		}
	}
	
	public FormToolkit getToolkit(Display display) {
		return null;
	}

}
