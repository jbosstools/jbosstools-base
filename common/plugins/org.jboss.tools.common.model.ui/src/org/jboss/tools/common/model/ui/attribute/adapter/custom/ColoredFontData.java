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
package org.jboss.tools.common.model.ui.attribute.adapter.custom;

import java.util.*;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.*;
import org.eclipse.swt.widgets.Display;
import org.jboss.tools.common.model.ui.ModelUIPlugin;

public class ColoredFontData {
	public static final ColoredFontData DEFAULT = new ColoredFontData();
	
	static {
		try {
			DEFAULT.data = Display.getDefault().getSystemFont().getFontData()[0];
			DEFAULT.color = Display.getDefault().getSystemColor(SWT.COLOR_BLACK).getRGB();
		} catch (SWTException e) {
			// FIXME See How to handle thos exception in PreferenceConverter Class
			ModelUIPlugin.getPluginLog().logError(e);
		}
	}

    FontData data;
    RGB color;

	public FontData getFontData() {
		return data;
	}
	
	public RGB getColor() {
		return color;
	}
	
	public static ColoredFontData toFontData(String text) {
		if(text == null || text.length() == 0) return DEFAULT;
		StringTokenizer st = new StringTokenizer(text, ","); //$NON-NLS-1$
		int q = 0;
		FontData data = new FontData(DEFAULT.getFontData().getName(), DEFAULT.getFontData().getHeight(), DEFAULT.getFontData().getStyle());
		RGB color = DEFAULT.getColor();
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			if(q == 0) {
				if(!"default".equals(t)) data.setName(t);  //$NON-NLS-1$
			} else if(t.startsWith("size=")) { //$NON-NLS-1$
				data.setHeight(getInt(t.substring(5), data.getHeight()));
			} else if(t.startsWith("style=")) { //$NON-NLS-1$
				data.setStyle(getInt(t.substring(6), data.getStyle()));
			} else if(t.startsWith("color=")) { //$NON-NLS-1$
				String sc = t.substring(6);
				int red = color.red, green = color.green, blue = color.blue;
				int i = sc.indexOf('-');
				if(i < 0) continue;
				red = getInt(sc.substring(0, i), red);
				sc = sc.substring(i + 1);
				i = sc.indexOf('-');
				if(i < 0) continue;
				green = getInt(sc.substring(0, i), green);
				sc = sc.substring(i + 1);
				blue = getInt(sc, blue);
				color = new RGB(red, green, blue);				
			}
			++q;
		}
		ColoredFontData d = new ColoredFontData();
		d.data = data;
		d.color = color;
		return d;
	}
	
	private static int getInt(String s, int def) {
		if(s == null || s.length() == 0) return def;
		try {
			return Integer.parseInt(s);
		} catch (NumberFormatException e) {
			ModelUIPlugin.getPluginLog().logError(e);
			return def;
		}
	}
	
	public static String toString(FontData data, RGB rgb) {
		StringBuffer sb = new StringBuffer();
		if(data.getName().equals(DEFAULT.getFontData().getName()))
			sb.append("default"); //$NON-NLS-1$
		else
			sb.append(data.getName());
		sb.append(",size=" + data.getHeight()); //$NON-NLS-1$
		sb.append(",style=" + data.getStyle()); //$NON-NLS-1$
		if(!DEFAULT.color.equals(rgb)) {
			sb.append(",color=" + rgb.red + "-" + rgb.green + "-" + rgb.blue); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
		} 
		return sb.toString();
	}

}
