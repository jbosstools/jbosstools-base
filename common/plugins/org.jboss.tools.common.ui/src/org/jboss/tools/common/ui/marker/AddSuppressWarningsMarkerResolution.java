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

import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IBuffer;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.ISourceReference;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jdt.internal.ui.preferences.ProblemSeveritiesConfigurationBlock;
import org.eclipse.jdt.ui.PreferenceConstants;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IMarkerResolution2;
import org.eclipse.ui.dialogs.PreferencesUtil;
import org.eclipse.ui.internal.dialogs.PropertyPageContributorManager;
import org.eclipse.ui.internal.dialogs.PropertyPageManager;
import org.eclipse.ui.internal.dialogs.PropertyPageNode;
import org.jboss.tools.common.EclipseUtil;
import org.jboss.tools.common.ui.CommonUIMessages;
import org.jboss.tools.common.ui.CommonUIPlugin;

/**
 * @author Daniel Azarov
 */
public class AddSuppressWarningsMarkerResolution implements
		IMarkerResolution2 {
	public static final String SPACE = " ";  //$NON-NLS-1$
	public static final String AT = "@";  //$NON-NLS-1$
	private static final String JAVA_COMPILER_ID="org.eclipse.jdt.ui.propertyPages.CompliancePreferencePage";  //$NON-NLS-1$
	private static final String PROBLEM_SEVERITIES_ID="org.eclipse.jdt.ui.propertyPages.ProblemSeveritiesPreferencePage";  //$NON-NLS-1$
	

	private IFile file;
	private IJavaElement element;
	private String preferenceKey;
	private String label;
	
	public AddSuppressWarningsMarkerResolution(IFile file, IJavaElement element, String preferenceKey){
		this.file = file;
		this.element = element;
		this.preferenceKey = preferenceKey;
		label = NLS.bind(CommonUIMessages.ADD_SUPPRESS_WARNINGS, element.getElementName());
	}

	public String getLabel() {
		return label;
	}

	public void run(IMarker marker) {
		try {
			ICompilationUnit original = EclipseUtil.getCompilationUnit(file);
			ICompilationUnit compilationUnit;
			compilationUnit = original.getWorkingCopy(new NullProgressMonitor());
			
			addAnnotation("SuppressWarnings(\""+preferenceKey+"\")", compilationUnit, element);
			
			compilationUnit.commitWorkingCopy(true, new NullProgressMonitor());
			compilationUnit.discardWorkingCopy();
		} catch (JavaModelException e) {
			CommonUIPlugin.getDefault().logError(e);
		} catch (CoreException e) {
			CommonUIPlugin.getDefault().logError(e);
		}
		
		JavaCore.getPlugin().getPluginPreferences().setValue(JavaCore.COMPILER_PB_UNHANDLED_WARNING_TOKEN, JavaCore.IGNORE);
		JavaCore.getPlugin().savePluginPreferences();
	}
	
	public String getDescription() {
		return label;
	}

	public Image getImage() {
		return JavaPlugin.getImageDescriptorRegistry().get(JavaPluginImages.DESC_OBJS_ANNOTATION);
	}
	
	public static void addAnnotation(String name, ICompilationUnit compilationUnit, IJavaElement element) throws JavaModelException{
		IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, element);
		if(workingCopyElement == null){
			return;
		}
		
		if(!(workingCopyElement instanceof ISourceReference))
			return;
		
		ISourceReference workingCopySourceReference = (ISourceReference) workingCopyElement;
		
		//IAnnotation annotation = findAnnotation(workingCopyMember, name);
		//if(annotation != null && annotation.exists())
		//	return;
		
		IBuffer buffer = compilationUnit.getBuffer();
		
		String str = AT+name;
		
		if(workingCopySourceReference instanceof IType){
			str += compilationUnit.findRecommendedLineSeparator();
		}else{
			str += SPACE;
		}
		
		buffer.replace(workingCopySourceReference.getSourceRange().getOffset(), 0, str);
	}
	
	@SuppressWarnings("unchecked")
	public static <T extends IJavaElement> T findWorkingCopy(ICompilationUnit compilationUnit, T element) throws JavaModelException{
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
	}}
