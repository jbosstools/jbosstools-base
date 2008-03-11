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
package org.jboss.tools.common.core.resources;

import org.eclipse.jdt.core.*;

import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.EclipseResourceUtil;


public class JavaFileAdaptable extends FileAnyImpl {
    private static final long serialVersionUID = 2045295535920063930L;
	
	public Object getAdapter(Class adapter) {
		if (adapter == ICompilationUnit.class) {
			IJavaElement element = EclipseResourceUtil.findJavaElement(this);
			if (element != null && element.getElementType() == IJavaElement.COMPILATION_UNIT) 
				return element; 
		}
		return super.getAdapter(adapter);
	}
}
