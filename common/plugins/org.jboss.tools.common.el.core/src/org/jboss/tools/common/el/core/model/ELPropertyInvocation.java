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
package org.jboss.tools.common.el.core.model;

import org.jboss.tools.common.el.core.parser.LexicalToken;

/**
 * 
 * @author V. Kabanovich
 *
 */
public interface ELPropertyInvocation extends ELInvocationExpression {

	public LexicalToken getSeparator();

	public LexicalToken getName();

	/**
	 * If all left expressions are also property invocations 
	 * (or variable names in terms of underlying model), 
	 * this method returns qualified name as 
	 * NAME (. NAME)*
	 * Otherwise, (i.e. if some left expression is method or 
	 * argument invocation), null is returned.
	 * 
	 * @return 
	 */
	public String getQualifiedName();

}
