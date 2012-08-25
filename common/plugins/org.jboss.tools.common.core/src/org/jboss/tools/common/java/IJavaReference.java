/******************************************************************************* 
 * Copyright (c) 2011 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/
package org.jboss.tools.common.java;

import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;

/**
 * @author Alexey Kazakov
 */
public interface IJavaReference {

	/**
	 * Returns the closest java member of this element.
	 * For example for local variable (method parameter) it will return the parent method.
	 * @return
	 */
	IMember getSourceMember();

	/**
	 * Returns java element. For java members this method will return the same object as getSourceMember();
	 * @return
	 */
	IJavaElement getSourceElement();
}