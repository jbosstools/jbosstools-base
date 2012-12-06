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
package org.jboss.tools.common.java.test;

import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.core.Annotation;
import org.eclipse.jdt.internal.core.CompilationUnit;
import org.eclipse.jdt.internal.core.MemberValuePair;
import org.eclipse.jdt.internal.core.SourceType;

/**
 * 
 * @author Viacheslav Kabanovich
 *
 */
public class FakeAnnotationImpl extends Annotation {
	
	public FakeAnnotationImpl() {
		super(null, "Fake");
	}

	@Override
	public boolean exists() {
		return true;
	}

	@Override
	public IJavaElement getAncestor(int ancestorType) {
		if(ancestorType == IJavaElement.COMPILATION_UNIT) {
			return new CompilationUnit(null, null, null) {
				public IImportDeclaration[] getImports() throws JavaModelException {
					return new IImportDeclaration[0];
				}
			};
		}
		if(ancestorType == IJavaElement.TYPE) {
			class FakeSourceType extends SourceType {
				FakeSourceType() {
					super(null, "FakeType");
				}
			}
			return new FakeSourceType();
		}
		return null;
	}

	@Override
	public String getSource() throws JavaModelException {
		return "@Fake({null})";
	}

	@Override
	public IMemberValuePair[] getMemberValuePairs() throws JavaModelException {
		IMemberValuePair pair = new MemberValuePair("value", new String[]{null}, IMemberValuePair.K_UNKNOWN);
		return new IMemberValuePair[]{pair};
	}

}
