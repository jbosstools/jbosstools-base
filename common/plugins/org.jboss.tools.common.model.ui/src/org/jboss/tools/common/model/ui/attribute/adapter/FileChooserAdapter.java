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
package org.jboss.tools.common.model.ui.attribute.adapter;

import java.util.*;
import org.jboss.tools.common.meta.*;
import org.jboss.tools.common.meta.constraint.impl.*;

public class FileChooserAdapter extends DefaultValueAdapter {
	protected Properties properties;
//	private String description = "";
//	private boolean all = false;
	private String[] extensions = null;
	private String[] filenames = null;

	public void dispose() {
		super.dispose();
		properties = null;
	}

	public void setAttribute(XAttribute attribute) {
		super.setAttribute(attribute);
		XAttributeConstraintFileFilter c = (XAttributeConstraintFileFilter)attribute.getConstraint();
		properties = c.getProperties();
		if(properties != null) {
//			description = properties.getProperty("description");
			String s = properties.getProperty("extensions");
			StringTokenizer st = new StringTokenizer((s == null) ? "" : s, ";,");
			extensions = new String[st.countTokens()];
			for (int i = 0; i < extensions.length; i++) {
			  extensions[i] = "*." + st.nextToken();
//			  if("*".equals(extensions[i])) all = true;
			}
			s = properties.getProperty("filenames");
			if (s != null) {
				st = new StringTokenizer(s, ";,");
				filenames = new String[st.countTokens()];
				for (int i = 0; i < filenames.length; i++)
					filenames[i] = st.nextToken();
				if(extensions == null || extensions.length == 0) extensions = filenames;
			}
		}		
	}
	
	public String[] getExtensions() {
		return extensions;
	}
	
	public String[] getFileNames()
	{
		return filenames;
	}
	
	public Object getAdapter(Class adapter) {
		if(adapter.isAssignableFrom(getClass())) return this;
		return super.getAdapter(adapter);	
	}
	
}
