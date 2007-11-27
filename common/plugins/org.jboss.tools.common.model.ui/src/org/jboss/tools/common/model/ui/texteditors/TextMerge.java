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
package org.jboss.tools.common.model.ui.texteditors;

import org.eclipse.jface.text.IDocument;

public class TextMerge {
	public static boolean replace(IDocument document, String text) {
		if(document == null) return false;
		String oldtext = document.get();
		if(oldtext == null || oldtext.length() == 0) return false;
		int b = getFirstDifference(oldtext, text);
		if(b < 0) return true;
		int e = getLastDifference(oldtext, text);
		if(e < 0) return false;
		++e;
		if(e < b) {
			e = b;
		} 
		int e2 = text.length() - oldtext.length() + e;
		if(e2 < b) {
			e = e + b - e2;
			e2 = b;
		}
		if(e == b) while(true) {
			if(b > 0 && oldtext.charAt(b - 1) == text.charAt(e2 - 1) && oldtext.charAt(b - 1) != '\n') {
				--e;
				--b;
				--e2;
			} else break;
		} else if(b == e2) while(true) {
			if(b > 0 && text.charAt(b - 1) == oldtext.charAt(e - 1) && text.charAt(b - 1) != '\n') {
				--e;
				--b;
				--e2;
			} else break;
		}
		try {
			document.replace(b, e - b, text.substring(b, e2));
		} catch (Exception exc) {
			return false;
		}
		return true;
	}
	
	static int getFirstDifference(String s1, String s2) {
		for (int i = 0; i < s1.length() && i < s2.length(); i++) {
			if(s1.charAt(i) != s2.charAt(i)) return i; 
		}
		return (s1.length() < s2.length()) ? s1.length() :
		       (s2.length() < s1.length()) ? s2.length() : -1;
	}
	
	static int getLastDifference(String s1, String s2) {
		for (int i1 = s1.length() - 1, i2 = s2.length() - 1; i1 >= 0 && i2 >= 0; i1--, i2--) {
			if(s1.charAt(i1) != s2.charAt(i2)) return i1;			
		}
		return -1;
	}

}
