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
 * 
 * @author V. Kabanovich
 *
 */
public enum ELObjectType {
	EL_UNKNOWN,
	EL_MODEL,
	EL_INSTANCE,
	EL_METHOD_INVOCATION,
	EL_QUALIFIED_NAME,
	EL_PROPERTY_INVOCATION,
	EL_ARGUMENT_INVOCATION,
	EL_OPERATOR,
	EL_VALUE,
	EL_COMPLEX_EXPRESSION,
	EL_MULTI_EXPRESSION,
	EL_ARGUMENT,
	EL_PARAMETERS;

}
