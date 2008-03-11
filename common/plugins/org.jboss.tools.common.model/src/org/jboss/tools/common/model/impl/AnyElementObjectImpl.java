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
package org.jboss.tools.common.model.impl;

import java.util.StringTokenizer;

import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;

public class AnyElementObjectImpl extends OrderedObjectImpl {
	private static final long serialVersionUID = 1L;
	public static char SEPARATOR = ';';

	public String getPresentationString() {
		return "" + get("tag");
	}

	public String name() {
		return "" + get("tag") + get(XModelObjectLoaderUtil.ATTR_ID_NAME);
	}
	
	public String[][] getAttributes() {
		String attrs = getAttributeValue("attributes");
		StringTokenizer st = new StringTokenizer(attrs, "" + SEPARATOR);
		int length = st.countTokens();
		String[][] as = new String[length][2];
		for (int i = 0; i < length; i++) {
			String t = st.nextToken();
			int k = t.indexOf('=');
			String n = k < 0 ? "" : t.substring(0, k);
			String v = t.substring(k + 1);
			as[i][0] = n;
			as[i][1] = v;
		}
		return as;
	}

}
