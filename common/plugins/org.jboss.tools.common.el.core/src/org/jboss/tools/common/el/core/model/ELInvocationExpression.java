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

/**
 * Generic interface for all kinds of expressions that involve
 * resolving variables, properties, methods etc. in underlying
 * model (i.e. Java model).
 * The important property of such an expression is that it 
 * can be associated to left operand that serves as scope 
 * for its resolution. If left operand is not explicitly set, 
 * it is implied.
 * 
 * @author V. Kabanovich
 *
 */
public interface ELInvocationExpression extends ELExpression {

	/**
	 * 
	 * @return Preceding expression, that serves
	 * as scope for resolving this expression,
	 * Two expressions are connected syntactically,
	 * for instance expr1.expr2 or expr1[expr2]
	 * or in other ways specified by language. 
	 */
	public ELInvocationExpression getLeft();

	public int getInvocationStartPosition();

	public String getMemberName();

}
