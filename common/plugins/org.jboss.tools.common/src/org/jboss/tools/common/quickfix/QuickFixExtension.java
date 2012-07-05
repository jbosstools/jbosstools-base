/******************************************************************************* 
 * Copyright (c) 2012 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.quickfix;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.CommonPlugin;

public class QuickFixExtension {
	public static String EXTENSION_POINT = "org.jboss.tools.common.quickFix"; //$NON-NLS-1$

	String id;
	IQuickFixGenerator generator;

	public QuickFixExtension() {}

	public String getId() {
		return id;
	}

	public IQuickFixGenerator getQuickFixGenerator() {
		return generator;
	}

	static QuickFixExtension[] INSTANCES;

	public static QuickFixExtension[] getInstances() {
		if(INSTANCES != null) return INSTANCES;
		List<QuickFixExtension> list = new ArrayList<QuickFixExtension>();
		IExtensionPoint point = Platform.getExtensionRegistry().getExtensionPoint(EXTENSION_POINT);
		IConfigurationElement[] es = point.getConfigurationElements();
		for (IConfigurationElement e: es) {
			QuickFixExtension n = new QuickFixExtension();
			n.id = e.getAttribute("id"); //$NON-NLS-1$
			try{
				n.generator = (IQuickFixGenerator)e.createExecutableExtension("generator-class"); //$NON-NLS-1$
			}catch(CoreException ex){
				CommonPlugin.getDefault().logError(ex);
			}
			list.add(n);
		}
		return INSTANCES = list.toArray(new QuickFixExtension[0]);
	}
}
