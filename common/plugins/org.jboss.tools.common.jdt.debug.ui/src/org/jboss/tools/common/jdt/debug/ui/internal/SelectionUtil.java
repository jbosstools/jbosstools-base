/*************************************************************************************
 * Copyright (c) 2015 Red Hat, Inc. and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     JBoss by Red Hat - Initial implementation.
 ************************************************************************************/
package org.jboss.tools.common.jdt.debug.ui.internal;

import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;

public class SelectionUtil {
	public static IJavaElement[] getJavaElements(ISelection sel) {
		Set<IJavaElement> elements = new LinkedHashSet<IJavaElement>(); 
		if( sel instanceof ITreeSelection) {
			List objects = ((ITreeSelection)sel).toList();
			for (Object object:objects) {
				if (!(object instanceof IJavaElement) && object instanceof IAdaptable) {
					object = ((IAdaptable) object).getAdapter(IJavaElement.class);
				}
				if( object instanceof IJavaElement) {
					elements.add((IJavaElement)object);
				}
			}
		}
		return (IJavaElement[]) elements.toArray(new IJavaElement[elements.size()]);
	}
	
	public static IResource getLaunchableResource(ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection ss = (IStructuredSelection) selection;
			if (ss.size() == 1) {
				Object selected = ss.getFirstElement();
				if (!(selected instanceof IJavaElement)
						&& selected instanceof IAdaptable) {
					selected = ((IAdaptable) selected)
							.getAdapter(IJavaElement.class);
				}
				if (selected instanceof IJavaElement) {
					return ((IJavaElement) selected).getResource();
				}
			}
		}
		return null;
	}
}
