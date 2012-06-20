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
import java.util.List;
import java.util.Map;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.ui.IMarkerResolution;
import org.eclipse.ui.IMarkerResolutionGenerator2;
import org.eclipse.wst.sse.ui.internal.reconcile.TemporaryAnnotation;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.text.xml.quickfix.IQuickFixGenerator;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.validation.ValidationErrorManager;

/**
 * @author Daniel Azarov
 */
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
						boolean enabled = marker.getAttribute(ValidationErrorManager.SUPPRESS_WARNINGS_ENABLED_ATTRIBUTE, false);
						int severity = marker.getAttribute(IMarker.SEVERITY, 0);
						if(enabled && severity == IMarker.SEVERITY_WARNING){
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
	
	private String getPreferenceKey(TemporaryAnnotation annotation){
		String attribute = (String)annotation.getAttributes().get(ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME);
		return attribute; 
	}

	private String getPreferencePageId(TemporaryAnnotation annotation){
		String attribute = (String)annotation.getAttributes().get(ValidationErrorManager.PREFERENCE_PAGE_ID_NAME);
		return attribute; 
	}

	@Override
	public boolean hasProposals(Annotation annotation) {
		if(annotation instanceof TemporaryAnnotation){
			return getPreferenceKey((TemporaryAnnotation)annotation) != null && getPreferencePageId((TemporaryAnnotation)annotation) != null;
		}
		return false;
	}

	@Override
	public List<ICompletionProposal> getProposals(Annotation annotation) {
		ArrayList<ICompletionProposal> proposals = new ArrayList<ICompletionProposal>();
		if(annotation instanceof TemporaryAnnotation){
			String preferenceKey = getPreferenceKey((TemporaryAnnotation)annotation);
			String preferencePageId = getPreferencePageId((TemporaryAnnotation)annotation);
			if(preferenceKey != null && preferencePageId != null){
				proposals.add(new ConfigureProblemSeverityMarkerResolution(preferencePageId, preferenceKey));
			}
		}
		return proposals;
	}

}
