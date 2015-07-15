/*******************************************************************************
 * Copyright (c) 2015 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.quickfix;

import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IField;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator2;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.refactoring.MarkerResolutionUtils;

abstract public class AbstractQuickFixGenerator implements IMarkerResolutionGenerator2, IQuickFixGenerator {
	protected static final String JAVA_EXTENSION = "java"; //$NON-NLS-1$
	protected static final String XML_EXTENSION = "xml"; //$NON-NLS-1$
	protected static final int MARKER_RESULUTION_NUMBER_LIMIT = 7;
	public static final String MESSAGE_ID_ATTRIBUTE_NAME = "Message_id"; //$NON-NLS-1$
	
	abstract protected IMarkerResolution[] findResolutions(IMarker marker) throws CoreException;

	@Override
	public IMarkerResolution[] getResolutions(IMarker marker) {
		try {
			return findResolutions(marker);
		} catch (CoreException ex) {
			CommonPlugin.getDefault().logError(ex);
		}
		return new IMarkerResolution[] {};

	}
	
	@Override
	public boolean hasResolutions(IMarker marker) {
		if(marker.exists()){
			try {
				return getMessageID(marker) >= 0;
			} catch (CoreException ex) {
				CommonPlugin.getDefault().logError(ex);
			}
		}
		return false;
	}
	
	protected int getMessageID(TemporaryAnnotation annotation){
		if(annotation.getAttributes() != null){
			Integer attribute = ((Integer) annotation.getAttributes().get(MESSAGE_ID_ATTRIBUTE_NAME));
			if (attribute != null)
				return attribute.intValue();
		}
		return -1; 
	}
	
	protected int getMessageID(IMarker marker)throws CoreException{
		Integer attribute = ((Integer) marker.getAttribute(MESSAGE_ID_ATTRIBUTE_NAME));
		if (attribute != null)
			return attribute.intValue();

		return -1; 
	}

	protected IJavaElement findJavaElementByQualifiedName(IJavaProject javaProject, String qualifiedName){
		try {
			return javaProject.findType(qualifiedName);
		} catch (JavaModelException ex) {
			CommonPlugin.getDefault().logError(ex);
		}
		
		return null;
	}
	
	protected IMethod findMethod(ICompilationUnit compilationUnit, int start){
		IJavaElement javaElement = findJavaElement(compilationUnit, start);
		if(javaElement instanceof IMethod){
			IMethod method = (IMethod)javaElement;
			if(!method.isBinary())
				return method;
		}
		return null;
	}
	
	protected IJavaElement findJavaElement(ICompilationUnit compilationUnit, int start){
		try{
			return compilationUnit == null ? null : compilationUnit.getElementAt(start);
		}catch(JavaModelException ex){
			CommonPlugin.getDefault().logError(ex);
		}
		return null;
		
	}
	
	protected ILocalVariable findParameter(ICompilationUnit compilationUnit, int start){
		IJavaElement element = findJavaElement(compilationUnit, start);
		if(element instanceof IMethod){
			try {
				for(ILocalVariable parameter : ((IMethod) element).getParameters()){
					if(parameter.getSourceRange().getOffset() <= start && parameter.getSourceRange().getOffset()+parameter.getSourceRange().getLength() > start){
						return parameter;
					}
				}
			} catch (JavaModelException ex) {
				CommonPlugin.getDefault().logError(ex);
			}
		}
		return null;
	}
	
	protected boolean isMethodExists(IType interfaceType, IMethod method){
		IMethod existingMethod = interfaceType.getMethod(method.getElementName(), method.getParameterTypes());
		if(existingMethod.exists())
			return true;
		return false;
	}
	
	protected IField findNonStaticField(ICompilationUnit compilationUnit, int start){
		try{
			IJavaElement javaElement = findJavaElement(compilationUnit, start);
			
			if(javaElement instanceof IField){
				IField field = (IField)javaElement;
				if(!Flags.isStatic(field.getFlags()) && !field.isBinary())
					return field;
			}
		}catch(JavaModelException ex){
			CommonPlugin.getDefault().logError(ex);
		}
		return null;
	}

	protected IField findPublicField(ICompilationUnit compilationUnit, int start){
		try{
			IJavaElement javaElement = findJavaElement(compilationUnit, start);
			
			if(javaElement instanceof IField){
				IField field = (IField)javaElement;
				if(Flags.isPublic(field.getFlags()) && !field.isBinary())
					return field;
			}
		}catch(JavaModelException ex){
			CommonPlugin.getDefault().logError(ex);
		}
		return null;
	}
	
	protected IAnnotation getAnnotation(IJavaElement element, String annotationQualifiedName){
		if(element instanceof IAnnotatable){
			String shortName = MarkerResolutionUtils.getShortName(annotationQualifiedName);
			IAnnotation[] annotations;
			try {
				annotations = ((IAnnotatable)element).getAnnotations();
				for(IAnnotation annotation : annotations){
					if(annotation.getElementName().equals(annotationQualifiedName) ||
							annotation.getElementName().equals(shortName))
						return annotation;
						
				}
			} catch (JavaModelException e) {
				CommonPlugin.getDefault().logError(e);
			}
		}
		return null;
	}
	
	protected IJavaElement findJavaElementByAnnotation(IJavaElement element, String qualifiedName) throws JavaModelException{
		IAnnotation annotation = getAnnotation(element, qualifiedName);
		if(annotation != null)
			return element;
		
		if(element instanceof IMethod){
			for(ILocalVariable parameter : ((IMethod)element).getParameters()){
				annotation = getAnnotation(parameter, qualifiedName);
				if(annotation != null)
					return parameter;
			}
		}else if(element instanceof ILocalVariable){
			IJavaElement parent = element.getParent();
			if(parent != null){
				annotation = getAnnotation(parent, qualifiedName);
				if(annotation != null)
					return parent;
			}
		}else if(element instanceof IType){
			for(IField field : ((IType)element).getFields()){
				annotation = getAnnotation(field, qualifiedName);
				if(annotation != null)
					return field;
			}
			
			for(IMethod method : ((IType)element).getMethods()){
				annotation = getAnnotation(method, qualifiedName);
				if(annotation != null)
					return method;
				for(ILocalVariable parameter : method.getParameters()){
					annotation = getAnnotation(parameter, qualifiedName);
					if(annotation != null)
						return parameter;
				}
			}
		}
		
		if(element instanceof IMember){
			annotation = getAnnotation(((IMember)element).getDeclaringType(), qualifiedName);
			if(annotation != null)
				return ((IMember)element).getDeclaringType();
		}
		
		return null;
	}

}
