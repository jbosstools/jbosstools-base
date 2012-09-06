/*******************************************************************************
 * Copyright (c) 2011 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.ui.marker;

import java.util.ArrayList;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.javaeditor.JavaMarkerAnnotation;
import org.eclipse.jdt.ui.text.java.IJavaCompletionProposal;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator2;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.quickfix.IQuickFixGenerator;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.validation.ValidationErrorManager;
import org.jboss.tools.common.validation.java.TempJavaProblemAnnotation;

/**
 * @author Daniel Azarov
 */
@SuppressWarnings("restriction")
public class ConfigureProblemSeverityResolutionGenerator implements
		IMarkerResolutionGenerator2, IQuickFixGenerator {

	@Override
	public IMarkerResolution[] getResolutions(IMarker marker) {
		ArrayList<IMarkerResolution> resolutions = new ArrayList<IMarkerResolution>();
		int position = marker.getAttribute(IMarker.CHAR_START, 0);
		try {
			if(marker.getResource() instanceof IFile){
				IFile file = (IFile)marker.getResource();
				if(file != null){
					String preferenceKey = getPreferenceKey(marker);
					String preferencePageId = getPreferencePageId(marker);
					if(preferenceKey != null && preferencePageId != null){
						int severity = marker.getAttribute(IMarker.SEVERITY, 0);
						if(severity == IMarker.SEVERITY_WARNING){
							IJavaElement element = findJavaElement(file, position);
							if(element != null){
								if(element instanceof IMethod){
									ILocalVariable parameter = findParameter((IMethod)element, position);
									if(parameter != null){
										resolutions.add(new AddSuppressWarningsMarkerResolution(file, parameter, preferenceKey));
									}
								}
								resolutions.add(new AddSuppressWarningsMarkerResolution(file, element, preferenceKey));
							}
						}
						resolutions.add(new ConfigureProblemSeverityMarkerResolution(preferencePageId, preferenceKey));
					}
				}
			}
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return resolutions.toArray(new IMarkerResolution[] {});
	}
	
	private IJavaElement findJavaElement(IFile file, int position){
		try {
			ICompilationUnit compilationUnit = EclipseUtil.getCompilationUnit(file);
			if(compilationUnit != null){
				return compilationUnit.getElementAt(position);
			}
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return null;
	}

	private IJavaElement findJavaElement(TempJavaProblemAnnotation annotation, int position){
		try {
			ICompilationUnit compilationUnit = annotation.getCompilationUnit();
			if(compilationUnit != null){
				return compilationUnit.getElementAt(position);
			}
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return null;
	}
	
	private ILocalVariable findParameter(IMethod method, int position) throws JavaModelException{
		for(ILocalVariable parameter : method.getParameters()){
			if(parameter.getSourceRange().getOffset() <= position && parameter.getSourceRange().getOffset()+parameter.getSourceRange().getLength() > position){
				return parameter;
			}
		}
		return null;
	}
	
	@Override
	public boolean hasResolutions(IMarker marker) {
		try {
			return getPreferenceKey(marker) != null && getPreferencePageId(marker) != null;
		} catch (CoreException ex) {
			CommonUIPlugin.getDefault().logError(ex);
		}
		return false;
	}
	
	private String getPreferenceKey(IMarker marker)throws CoreException{
		String attribute = marker.getAttribute(ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME, null);
		return attribute; 
	}

	private String getPreferencePageId(IMarker marker)throws CoreException{
		String attribute = marker.getAttribute(ValidationErrorManager.PREFERENCE_PAGE_ID_NAME, null);
		return attribute; 
	}
	
	private String getPreferenceKey(Annotation annotation){
		if(annotation instanceof TemporaryAnnotation){
			if(((TemporaryAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TemporaryAnnotation)annotation).getAttributes().get(ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME);
				return attribute;
			}
		}else if(annotation instanceof TempJavaProblemAnnotation){
			if(((TempJavaProblemAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TempJavaProblemAnnotation)annotation).getAttributes().get(ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME);
				return attribute;
			}
		}
		return null;
	}

	private String getPreferencePageId(Annotation annotation){
		if(annotation instanceof TemporaryAnnotation){
			if(((TemporaryAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TemporaryAnnotation)annotation).getAttributes().get(ValidationErrorManager.PREFERENCE_PAGE_ID_NAME);
				return attribute; 
			}
		}else if(annotation instanceof TempJavaProblemAnnotation){
			if(((TempJavaProblemAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TempJavaProblemAnnotation)annotation).getAttributes().get(ValidationErrorManager.PREFERENCE_PAGE_ID_NAME);
				return attribute; 
			}
		}
		return null;
	}

	@Override
	public boolean hasProposals(Annotation annotation, Position position) {
		return getPreferenceKey(annotation) != null && getPreferencePageId(annotation) != null;
	}

	@Override
	public IJavaCompletionProposal[] getProposals(Annotation annotation, Position position) {
		ArrayList<IJavaCompletionProposal> proposals = new ArrayList<IJavaCompletionProposal>();
		String preferenceKey = getPreferenceKey(annotation);
		String preferencePageId = getPreferencePageId(annotation);
		if(preferenceKey != null && preferencePageId != null){
			if(annotation instanceof TempJavaProblemAnnotation){
				TempJavaProblemAnnotation tAnnotation = (TempJavaProblemAnnotation)annotation;
				if(JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE.equals(tAnnotation.getType())){
					int offset = position.getOffset();
					IFile file = getFile(tAnnotation);
					if(file != null){
						IJavaElement element = findJavaElement(tAnnotation, offset);
						if(element != null){
							if(element instanceof IMethod){
								try{
									ILocalVariable parameter = findParameter((IMethod)element, offset);
									if(parameter != null){
										proposals.add(new AddSuppressWarningsMarkerResolution(file, parameter, preferenceKey, tAnnotation.getCompilationUnit()));
									}
								}catch(JavaModelException ex){
									CommonUIPlugin.getDefault().logError(ex);
								}
							}
							proposals.add(new AddSuppressWarningsMarkerResolution(file, element, preferenceKey, tAnnotation.getCompilationUnit()));
						}
					}
				}
			}
			proposals.add(new ConfigureProblemSeverityMarkerResolution(preferencePageId, preferenceKey));
		}
		return proposals.toArray(new IJavaCompletionProposal[]{});
	}
	
	
	private IFile getFile(TempJavaProblemAnnotation annotation){
		if(annotation.getCompilationUnit() != null){
			try {
				return (IFile)annotation.getCompilationUnit().getUnderlyingResource();
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
		}
		return null;
	}

}
