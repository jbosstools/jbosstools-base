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

import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IBuffer;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.preferences.SeverityPreferences;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;
import org.osgi.service.prefs.BackingStoreException;

/**
 * @author Daniel Azarov
 */
public class AddSuppressWarningsMarkerResolution implements
		IMarkerResolution2 {
	public static final String SUPPRESS_WARNINGS_ANNOTATION = "SuppressWarnings";
	public static final String SPACE = " ";  //$NON-NLS-1$
	public static final String AT = "@";  //$NON-NLS-1$
	private static final String PROBLEM_ID = JavaCore.COMPILER_PB_UNHANDLED_WARNING_TOKEN;
	private SP preferences = new SP();

	private IFile file;
	private IAnnotatable element;
	private String preferenceKey;
	private String label;
	
	public AddSuppressWarningsMarkerResolution(IFile file, IJavaElement element, String preferenceKey){
		this.file = file;
		this.element = getAnnatatableElement(element);
		this.preferenceKey = preferenceKey;
		label = NLS.bind(CommonUIMessages.ADD_SUPPRESS_WARNINGS, element.getElementName());
	}
	
	private IAnnotatable getAnnatatableElement(IJavaElement element){
		IJavaElement elem = element;
		while(elem != null){
			if(element instanceof IAnnotatable){
				return (IAnnotatable) element;
			}
			elem = elem.getParent();
		}
		return null;
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public void run(IMarker marker) {
		if(element != null){
			disablePreference();
			try {
				ICompilationUnit original = EclipseUtil.getCompilationUnit(file);
				ICompilationUnit compilationUnit = original.getWorkingCopy(new NullProgressMonitor());
				
				IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, (IJavaElement)element);
				
				IAnnotation annotation = findAnnotation(workingCopyElement);
				boolean status = false;
				if(annotation != null){
					status = updateAnnotation(SUPPRESS_WARNINGS_ANNOTATION, preferenceKey, compilationUnit, annotation);
				}else{
					status = addAnnotation(SUPPRESS_WARNINGS_ANNOTATION+"(\""+preferenceKey+"\")", compilationUnit, workingCopyElement);
				}
				
				if(status){
					compilationUnit.commitWorkingCopy(true, new NullProgressMonitor());
				}
				compilationUnit.discardWorkingCopy();
			} catch (JavaModelException e) {
				CommonUIPlugin.getDefault().logError(e);
			} catch (CoreException e) {
				CommonUIPlugin.getDefault().logError(e);
			}
		}
	}
	
	private IAnnotation findAnnotation(IJavaElement workingCopyElement) throws JavaModelException{
		IAnnotation annotation = ((IAnnotatable)workingCopyElement).getAnnotation(SUPPRESS_WARNINGS_ANNOTATION);
		if(annotation != null && annotation.exists()){
			return annotation;
		}
		
		return null;
	}
	
	private void disablePreference(){
		String value = preferences.getProjectPreference(file.getProject(), PROBLEM_ID);
		if(!SeverityPreferences.IGNORE.equals(value)){
			
			IEclipsePreferences projectPreferences = preferences.getProjectPreferences(file.getProject());
			String projectValue = null;
			if(projectPreferences != null){
				projectValue = projectPreferences.get(PROBLEM_ID, null);
			}
			
			if(projectValue != null){
				 MessageDialog dialog = new MessageDialog(getShell(), label, null,
							"Do you want to disable 'Unsupported @SuppressWarnings' error/warning",
							MessageDialog.QUESTION_WITH_CANCEL,
							new String[]{"Cancel", "Disable"},
							0);
					int result = dialog.open();
					if(result == 1){
						IEclipsePreferences ePrefs = preferences.getProjectPreferences(file.getProject());
						ePrefs.put(PROBLEM_ID, SeverityPreferences.IGNORE);
						try {
							ePrefs.flush();
						} catch (BackingStoreException e) {
							CommonUIPlugin.getDefault().logError(e);
						}
					}
			}else{
				 MessageDialog dialog = new MessageDialog(getShell(), label, null,
						"Do you want to disable 'Unsupported @SuppressWarnings' error/warning on the Workspace or only on the project '"+file.getProject().getName()+"'",
						MessageDialog.QUESTION_WITH_CANCEL,
						new String[]{"Cancel", "Workspace", file.getProject().getName()},
						0);
				int result = dialog.open();
				if(result == 1){
					IEclipsePreferences ePrefs = preferences.getInstancePreferences();
					ePrefs.put(PROBLEM_ID, SeverityPreferences.IGNORE);
					try {
						ePrefs.flush();
					} catch (BackingStoreException e) {
						CommonUIPlugin.getDefault().logError(e);
					}
				}else if(result == 2){
					IEclipsePreferences ePrefs = preferences.getProjectPreferences(file.getProject());
					ePrefs.put(PROBLEM_ID, SeverityPreferences.IGNORE);
					try {
						ePrefs.flush();
					} catch (BackingStoreException e) {
						CommonUIPlugin.getDefault().logError(e);
					}
				}
			}
		}
	}
	
	private static Shell getShell() {
		IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
		if (window == null) {
			IWorkbenchWindow[] windows = PlatformUI.getWorkbench().getWorkbenchWindows();
			if (windows.length > 0) {
				return windows[0].getShell();
			}
		}
		else {
			return window.getShell();
		}
		return null;
	}
	@Override
	public String getDescription() {
		return label;
	}

	@Override
	public Image getImage() {
		return JavaPlugin.getImageDescriptorRegistry().get(JavaPluginImages.DESC_OBJS_ANNOTATION);
	}
	
	private boolean updateAnnotation(String name, String parameter, ICompilationUnit compilationUnit, IAnnotation annotation) throws JavaModelException{
		IBuffer buffer = compilationUnit.getBuffer();
		
		String str = AT+name;
		
		str += "({";
		
		for(IMemberValuePair pair : annotation.getMemberValuePairs()){
			if(pair.getValue().toString().equals(parameter)){
				return false;
			}
			str += "\""+pair.getValue()+"\", ";
		}
		
		str += "\""+parameter+"\"";
		
		str += "})";
		
		buffer.replace(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength(), str);
		
		return true;
	}

	private boolean addAnnotation(String name, ICompilationUnit compilationUnit, IJavaElement element) throws JavaModelException{
		if(!(element instanceof ISourceReference))
			return false;
		
		ISourceReference workingCopySourceReference = (ISourceReference) element;
		
		IBuffer buffer = compilationUnit.getBuffer();
		
		String str = AT+name;
		
		if(!(workingCopySourceReference instanceof ILocalVariable)){
			str += compilationUnit.findRecommendedLineSeparator();
		}else{
			str += SPACE;
		}
		
		buffer.replace(workingCopySourceReference.getSourceRange().getOffset(), 0, str);
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private static <T extends IJavaElement> T findWorkingCopy(ICompilationUnit compilationUnit, T element) throws JavaModelException{
		if(element instanceof IAnnotation){
			IJavaElement parent = findWorkingCopy(compilationUnit, element.getParent());
			if(parent instanceof IAnnotatable){
				for(IAnnotation a : ((IAnnotatable)parent).getAnnotations()){
					if(a.getElementName().equals(element.getElementName()))
						return (T)a;
				}
			}
		}else if(element instanceof ILocalVariable && ((ILocalVariable) element).isParameter()){
			IJavaElement parent = findWorkingCopy(compilationUnit, element.getParent());
			if(parent instanceof IMethod){
				for(ILocalVariable parameter : ((IMethod)parent).getParameters()){
					if(parameter.getElementName().equals(element.getElementName()) && parameter.getTypeSignature().equals(((ILocalVariable)element).getTypeSignature()))
						return (T)parameter;
				}
			}
		}else{
			IJavaElement[] elements = compilationUnit.findElements(element);
			if(elements != null){
				for(IJavaElement e : elements){
					if(e.getClass().equals(element.getClass()))
						return (T)e;
				}
			}
		}
		return null;
	}
	
	static class SP extends SeverityPreferences{

		@Override
		protected Set<String> getSeverityOptionNames() {
			return null;
		}

		@Override
		protected String createSeverityOption(String shortName) {
			return null;
		}

		@Override
		protected String getPluginId() {
			return JavaCore.PLUGIN_ID;
		}
		
	}
	
}
