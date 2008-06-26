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
package org.jboss.tools.common.core.jdt;

import java.util.Collection;

import org.eclipse.jdt.core.search.TypeNameRequestor;


public class TypeInfoRequestor extends TypeNameRequestor {
	
	public TypeInfoRequestor(Collection typesFound) {
		super(); // TODO-3.3: why is constructor taking typesFound as argument ? If API in 3.3 change why is the constructor here not changed ? 
	}

	protected boolean inScope(char[] packageName, char[] typeName) {
		return true;
	}
	
}
