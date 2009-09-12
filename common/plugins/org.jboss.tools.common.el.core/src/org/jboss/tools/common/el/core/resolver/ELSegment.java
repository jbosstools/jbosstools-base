/******************************************************************************* 
 * Copyright (c) 2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.resolver;

import org.eclipse.jdt.core.IJavaElement;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;

/**
 * Describes a segment of EL operand.
 * @author Alexey Kazakov
 */
public interface ELSegment {

	/**
	 * @return source EL token.
	 */
	ELInvocationExpression getToken();

	/**
	 * @return member info object of resolved segment. May return null.
	 */
	TypeInfoCollector.MemberInfo getMemberInfo();

	/**
	 * @return Java Element which represent this resolve segment. May return null. 
	 */
	IJavaElement getJavaElement();

	/**
	 * @return true if the segment has been resolved.
	 */
	boolean isResolved();
}