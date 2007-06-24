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
package org.jboss.tools.common.model.ui.util;

import org.eclipse.swt.graphics.Drawable;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.widgets.Control;

public class StringUtilities {
   
	public static int getStringWidth(Drawable drawable, Font font, String str) {
		GC gc = new GC(drawable);
		gc.setFont(font);
		int width = gc.stringExtent(str).x;
		gc.dispose();
		return width;
	}
   
	public static int getStringWidth(Control control, String str) {
		return getStringWidth(control, control.getFont(), str);
	}
   
	public static String makeShortString(String source, int width, String sufix, Drawable drawable, Font font) {
		GC gc = new GC(drawable);
		gc.setFont(font);

		if (source == null) {
			source = "";
		}
		String str;
		if (gc.stringExtent(source).x > width) {
			if (sufix == null) {
				sufix = "";
			}
			str = sufix; 
			for (int i = 0; i < source.length(); i++) {
				String tmp = source.substring(0, i) + sufix;
				if (gc.stringExtent(tmp).x > width) {
					break; 
				}
				str = tmp;
			}
		} else {
			str = new String(source);
		}
		gc.dispose();
		return str;
	}
   
	public static String makeShortString(String source, int width, String sufix, Control control) {
		return makeShortString(source, width, sufix, control, control.getFont());
	}
   
	public static String dottedString(String source, int width, Drawable drawable, Font font) {
		return makeShortString(source, width, "...", drawable, font);
	}
   
	public static String dottedString(String source, int width, Control control) {
		return dottedString(source, width, control, control.getFont());
	}
}
