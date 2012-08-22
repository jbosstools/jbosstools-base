/******************************************************************************* 
 * Copyright (c) 2007-2010 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.ca;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.jboss.tools.common.el.core.resolver.IRelevanceCheck;

public class DefaultJavaRelevanceCheck implements IRelevanceCheck {
	String test1 = null;
	String test2 = null;
	String test3 = null;
	boolean isIType = false;

	public DefaultJavaRelevanceCheck(IJavaElement element) {
		test1 = element.getElementName();
		if (element instanceof IMethod) {
			if(test1.length() > 3 && (test1.startsWith("get") || test1.startsWith("set"))) { //$NON-NLS-1$ //$NON-NLS-2$
				test2 = test1.substring(3, 4).toLowerCase() + test1.substring(4);
				test3 = test1.substring(3);
			} else if(test1.length() > 2 && test1.startsWith("is")) { //$NON-NLS-1$
				test2 = test1.substring(2, 3).toLowerCase() + test1.substring(3);
				test3 = test1.substring(2);
			}
			if(test3.equals(test2)) {
				test3 = null;
			}
		} else if (element instanceof IType){
			isIType = true;
		}
	}
	
	public boolean isRelevant(String content) {
		return isIType 
				|| content.contains(test1) 
				|| test2 != null && content.contains(test2)
				|| test3 != null && content.contains(test3);
	}
}
