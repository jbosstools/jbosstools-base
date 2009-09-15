/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.common.model.options.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.model.util.XModelObjectUtil;

public class XStudioContributions {
	public static String EXTENSION_POINT = "org.jboss.tools.common.model.preferences"; //$NON-NLS-1$
	
	public static String ATTR_RESOURCE = "resource"; //$NON-NLS-1$
	public static String ATTR_TARGET = "target"; //$NON-NLS-1$
	public static String ATTR_PRIORITY = "priority"; //$NON-NLS-1$

	public static XStudioContribution[] getContributions() {
		List<XStudioContribution> result = new ArrayList<XStudioContribution>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] cs = point.getConfigurationElements();
		for (int i = 0; i < cs.length; i++) {
			XStudioContribution c = new XStudioContribution();
			c.resource = cs[i].getAttribute(ATTR_RESOURCE);
			c.loader = cs[i].getNamespaceIdentifier();
			if(c.loader == null) continue;
			String targets = cs[i].getAttribute(ATTR_TARGET);
			if(targets != null && targets.length() > 0) {
				c.targets = XModelObjectUtil.asStringArray(targets);
			}
			String pr = cs[i].getAttribute(ATTR_PRIORITY);
			if(pr != null && pr.length() > 0) {
				try {
					c.priority = Integer.parseInt(pr);
				} catch (NumberFormatException e) {
					c.priority = 100;
				}
			} else {
				c.priority = 10;
			}
			result.add(c);
			
		}
		XStudioContribution[] rs = result.toArray(new XStudioContribution[0]);
		Arrays.sort(rs, new C());
		return rs;
	}

	static class C implements Comparator<XStudioContribution> {

		public int compare(XStudioContribution o1, XStudioContribution o2) {
			return o1.priority - o2.priority;
		}
		
	}
	
}
