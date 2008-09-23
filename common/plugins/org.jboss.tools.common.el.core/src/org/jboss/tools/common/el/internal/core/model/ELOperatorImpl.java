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
package org.jboss.tools.common.el.internal.core.model;

import org.jboss.tools.common.el.core.model.ELObjectType;

/**
 * 
 * @author V. Kabanovich
 *
 */
public class ELOperatorImpl extends ELObjectImpl {

	public ELOperatorImpl() {}

	public void addChild(ELObjectImpl child) {
	}

	public String toString() {
		return getFirstToken() != null ? getFirstToken().getText() : "";
	}

	public ELObjectType getType() {
		return ELObjectType.EL_OPERATOR;
	}

}
