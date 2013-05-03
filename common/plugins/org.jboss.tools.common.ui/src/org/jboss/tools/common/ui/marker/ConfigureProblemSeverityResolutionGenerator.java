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
import org.eclipse.core.resources.IResource;
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
import org.jboss.tools.common.refactoring.MarkerResolutionUtils;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.jboss.tools.common.validation.IPreferenceInfo;
import org.jboss.tools.common.validation.PreferenceInfoManager;
import org.jboss.tools.common.validation.TempMarkerManager;
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
			IResource resource = marker.getResource();
			String preferenceKey = getPreferenceKey(marker);
			String markerType = getProblemType(marker);
			IPreferenceInfo info = PreferenceInfoManager.getPreferenceInfo(markerType);
			if(hasResolutions(preferenceKey, markerType, info, resource)){
				String propertyPageId = info.getPropertyPageId();
				String preferencePageId = info.getPreferencePageId();
				String pluginId = info.getPluginId();
				if(resource instanceof IFile) {
					int severity = marker.getAttribute(IMarker.SEVERITY, 0);
					if(severity == IMarker.SEVERITY_WARNING){
						IJavaElement element = findJavaElement((IFile)resource, position);
						if(element != null){
							if(element instanceof IMethod){
								ILocalVariable parameter = findParameter((IMethod)element, position);
								if(parameter != null){
								resolutions.add(new AddSuppressWarningsMarkerResolution((IFile)resource, parameter, preferenceKey));
								}
							}
							resolutions.add(new AddSuppressWarningsMarkerResolution((IFile)resource, element, preferenceKey));
						}
					}
				}
				resolutions.add(new ConfigureProblemSeverityMarkerResolution(resource.getProject(), preferencePageId, propertyPageId, preferenceKey, pluginId));
			}
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return resolutions.toArray(new IMarkerResolution[resolutions.size()]);
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
	
	private boolean hasResolutions(String preferenceKey, String markerType, IPreferenceInfo markerInfo, IResource resource){
		return preferenceKey != null && markerType != null && markerInfo != null;
	}
	
	@Override
	public boolean hasResolutions(IMarker marker) {
		try {
			String preferenceKey = getPreferenceKey(marker);
			String markerType = getProblemType(marker);
			IPreferenceInfo info = PreferenceInfoManager.getPreferenceInfo(markerType);
			IResource resource = marker.getResource();
			
			return hasResolutions(preferenceKey, markerType, info, resource);
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		return false;
	}
	
	private String getPreferenceKey(IMarker marker)throws CoreException{
		return marker.getAttribute(ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME, null);
	}

	private String getProblemType(IMarker marker)throws CoreException{
		if(marker.exists()){
			return marker.getType();
		}else{
			return null;
		}
	}

	private String getAttribute(Annotation annotation, String attributeName){
		if(annotation instanceof TemporaryAnnotation){
			if(((TemporaryAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TemporaryAnnotation)annotation).getAttributes().get(attributeName);
				return attribute;
			}
		}else if(annotation instanceof TempJavaProblemAnnotation){
			if(((TempJavaProblemAnnotation)annotation).getAttributes() != null){
				String attribute = (String)((TempJavaProblemAnnotation)annotation).getAttributes().get(attributeName);
				return attribute;
			}
		}
		return null;
	}
	
	@Override
	public boolean hasProposals(Annotation annotation, Position position) {
		String preferenceKey = getAttribute(annotation, ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME);
		String problemType = getAttribute(annotation, TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME);
		IPreferenceInfo info = PreferenceInfoManager.getPreferenceInfo(problemType);
		
		return preferenceKey != null && problemType != null && info != null;
	}

	@Override
	public IJavaCompletionProposal[] getProposals(Annotation annotation, Position position) {
		ArrayList<IJavaCompletionProposal> proposals = new ArrayList<IJavaCompletionProposal>();
		String preferenceKey = getAttribute(annotation, ValidationErrorManager.PREFERENCE_KEY_ATTRIBUTE_NAME);
		String problemType = getAttribute(annotation, TempMarkerManager.MESSAGE_TYPE_ATTRIBUTE_NAME);
		IPreferenceInfo info = PreferenceInfoManager.getPreferenceInfo(problemType);
		if(info == null){
			return new IJavaCompletionProposal[0];
		}
		String preferencePageId = info.getPreferencePageId();
		String propertyPageId = info.getPropertyPageId();
		String pluginId = info.getPluginId();
		IFile file = null;
		if(annotation instanceof TempJavaProblemAnnotation){
			TempJavaProblemAnnotation tAnnotation = (TempJavaProblemAnnotation)annotation;
			
			try {
				file = (IFile) tAnnotation.getCompilationUnit().getUnderlyingResource();
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
			if(file != null){
				if(JavaMarkerAnnotation.WARNING_ANNOTATION_TYPE.equals(tAnnotation.getType())){
					int offset = position.getOffset();
					
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
		}else{
			file = MarkerResolutionUtils.getFile();
		}
		if(file != null){
			proposals.add(new ConfigureProblemSeverityMarkerResolution(file.getProject(), preferencePageId, propertyPageId, preferenceKey, pluginId));
		}
		return proposals.toArray(new IJavaCompletionProposal[proposals.size()]);
	}
}
