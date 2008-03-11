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
package org.jboss.tools.common.meta.key;

import java.util.*;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.action.XAttributeData;

public class WizardKeys {
	private static Properties keys = new Properties(); 
	private static Properties labels = keys;
	private static Properties common = keys;
	
	static {
		Locale locale = Locale.getDefault();
		String localeString = locale.toString();
		while(true) {
			addNew(keys, KeyLoader.load(localeString));
			if(localeString.length() == 0) break;
			int i = localeString.indexOf('_');
			localeString = (i < 0) ? "" : localeString.substring(0, i);
		}
	}
	
	private static void addNew(Properties target, Properties source) {
		Enumeration it = source.keys();
		while(it.hasMoreElements()) {
			String n = it.nextElement().toString();
			if(target.containsKey(n)) continue;
			target.setProperty(n, source.getProperty(n));
		}
	}
	
	public static String getHeader(String key) {
		if(key == null) return null;
		return keys.getProperty(key + ".WindowTitle");
	}

	public static String getTitle(String key) {
		if(key == null) return null;
		return keys.getProperty(key + ".Title");
	}

	public static String getMessage(String key) {
		if(key == null) return null;
		return keys.getProperty(key + ".Message");
	}

	public static String getLabelText(String entity, String attribute) {
		String key = "" + entity + "." + attribute.replace(' ', '_');
		return keys.getProperty(key);
	}

	public static String getLabelText(String key) {
		return (key == null) ? null : labels.getProperty(key);
	}

	public static String getString(String key) {
		if (key!=null) {
			if (common.getProperty(key)!=null) {
				return common.getProperty(key);
			} else {
				return null;
			}
		}
		return "@NULL_KEY@";
		
		//return (key == null) ? null : common.getProperty(key);
	}

	public static String getAttributeDisplayName(XAttributeData a, boolean build) {
		String d = getAttributeDisplayName(a);
		return (d != null || !build) ? d : toDisplayName(a.getAttribute().getName());
	}

	public static String getAttributeDisplayName(XAttributeData a) {
		return getAttributeDisplayName(a.getAttribute());
	}

	public static String getAttributeDisplayName(XAttribute a, boolean build) {
		String d = getAttributeDisplayName(a);
		return (d != null || !build) ? d : toDisplayName(a.getName());
	}

	public static String getAttributeDisplayName(XAttribute a) {
		String s = getLabelText(a.getModelEntity().getName(), a.getName());
		if(s == null) {
			String labelText = a.getModelEntity().getName() + "_" + a.getName();
			s = getLabelText(labelText);
		}
		return s;
	}
	
	private static Set<String> LOWER_CASE_WORDS = new HashSet<String>();
	private static Set<String> UPPER_CASE_WORDS = new HashSet<String>();
	
	static {
		String[] lcw = new String[]{"the", "web.xml", "for"};
		String[] ucw = new String[]{"tld", "uri", "jsp", "html", "url", "jsf", "xml"};
		for (int i = 0; i < lcw.length; i++) LOWER_CASE_WORDS.add(lcw[i]);
		for (int i = 0; i < ucw.length; i++) UPPER_CASE_WORDS.add(ucw[i]);
	}
	
	public static String toDisplayName(String n) {
		if(n == null || n.length() == 0) return "";
		StringTokenizer st = new StringTokenizer(n, " -", true);
		StringBuffer sb = new StringBuffer();
		int k = st.countTokens();
		for (int i = 0; i < k; i++) {
			String t = st.nextToken();
			if(i > 0 && (t.length() < 3 || LOWER_CASE_WORDS.contains(t))) {
				sb.append(t);
			} else if(UPPER_CASE_WORDS.contains(t)) {
				sb.append(t.toUpperCase());
			} else {
				sb.append(t.substring(0, 1).toUpperCase()).append(t.substring(1));
			}
		}
		return sb.toString();
	}

}
