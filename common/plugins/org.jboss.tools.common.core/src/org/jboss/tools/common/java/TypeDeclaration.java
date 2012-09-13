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
package org.jboss.tools.common.java;

import org.eclipse.core.resources.IResource;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class TypeDeclaration extends ParametedType implements ITypeDeclaration {
	IResource resource;
	int length;
	int startPosition;
	Lazy lazy = null;
	
	public static interface Lazy {
		void init(TypeDeclaration d);
	}

	public TypeDeclaration(ParametedType type, IResource resource, Lazy lazy) {
		this(type, resource, 0, 0);
		this.lazy = lazy;
	}

	public TypeDeclaration(ParametedType type, IResource resource, int startPosition, int length) {
		this.setFactory(type.getFactory());
		this.type = type.getType();
		arrayIndex = type.arrayIndex;
		this.resource = resource;
		this.length = length;
		this.startPosition = startPosition;

		signature = type.signature;
		parameterTypes = type.parameterTypes;

		allInheritedTypes = type.allInheritedTypes;
		inheritanceIsBuilt = type.inheritanceIsBuilt;
		inheritedTypes = type.inheritedTypes;
		superType = type.superType;
		primitive = type.primitive;
		
		isLower = type.isLower;
		isUpper = type.isUpper;
		isVariable = type.isVariable;
		if(type instanceof TypeDeclaration) {
			this.lazy = ((TypeDeclaration)type).lazy;
		}
	}

	public void init(int startPosition, int length) {
		this.startPosition = startPosition;
		this.length = length;
	}

	private void init() {
		if(lazy != null) {
			synchronized (this) {
				lazy.init(this);
				lazy = null;
			}
		}
	}

	public int getLength() {
		init();
		return length;
	}

	public int getStartPosition() {
		init();
		return startPosition;
	}

	public IResource getResource() {
		return resource;
	}
}
