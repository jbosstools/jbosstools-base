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
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.refactoring.CompilationUnitChange;
import org.eclipse.jdt.internal.core.JavaElement;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jpt.common.core.internal.utility.jdt.ASTTools;
import org.eclipse.osgi.util.NLS;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.ReplaceEdit;
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
	public static final String DOT = ".";  //$NON-NLS-1$
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
		this.preferenceKey = getShortName(preferenceKey);;
		label = NLS.bind(CommonUIMessages.ADD_SUPPRESS_WARNINGS_TITLE, this.preferenceKey, element.getElementName());
		if(element instanceof IMethod){
			label += "()";
		}
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
				
				CompilationUnit cuNode = ASTTools.buildASTRoot(compilationUnit);
				
				IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, (IJavaElement)element);
				ASTNode elementNode = null;
				if(workingCopyElement instanceof JavaElement){
					elementNode = ((JavaElement) workingCopyElement).findNode(cuNode);
				}
				
				IAnnotation annotation = findAnnotation(workingCopyElement);
				CompilationUnitChange change = null;
				if(annotation != null){
					change = updateAnnotation(SUPPRESS_WARNINGS_ANNOTATION, preferenceKey, compilationUnit, annotation);
				}else{
					change = addAnnotation(SUPPRESS_WARNINGS_ANNOTATION+"(\""+preferenceKey+"\")", compilationUnit, workingCopyElement, elementNode);
				}
				
				if(change != null){
					change.perform(new NullProgressMonitor());
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
	
	public static String getShortName(String qualifiedName){
		int lastDot = qualifiedName.lastIndexOf(DOT);
		String name;
		if(lastDot < 0)
			name = qualifiedName;
		else
			name = qualifiedName.substring(lastDot+1);
		return name;
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
						 CommonUIMessages.ADD_SUPPRESS_WARNINGS_MESSAGE+
						 CommonUIMessages.ADD_SUPPRESS_WARNINGS_QUESTION1,
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
						 CommonUIMessages.ADD_SUPPRESS_WARNINGS_MESSAGE+
						NLS.bind(CommonUIMessages.ADD_SUPPRESS_WARNINGS_QUESTION2, file.getProject().getName()),
						MessageDialog.QUESTION_WITH_CANCEL,
						new String[]{"Cancel", "Workspace", "Project"},
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
	
	private CompilationUnitChange updateAnnotation(String name, String parameter, ICompilationUnit compilationUnit, IAnnotation annotation) throws JavaModelException{
		String str = AT+name;
		
		str += "({";
		
		for(IMemberValuePair pair : annotation.getMemberValuePairs()){
			if(pair.getValueKind() == IMemberValuePair.K_STRING){
				Object value = pair.getValue();
				if(value instanceof String){
					if(value.toString().equals(parameter)){
						return null;
					}
					str += "\""+value+"\", ";
				}else if(value instanceof Object[]){
					Object[] array = (Object[])value;
					for(Object a : array){
						if(a instanceof String){
							if(a.toString().equals(parameter)){
								return null;
							}
							str += "\""+a+"\", ";
						}
					}
				}
			} else if(pair.getValueKind() == IMemberValuePair.K_QUALIFIED_NAME || pair.getValueKind() == IMemberValuePair.K_SIMPLE_NAME){
				Object value = pair.getValue();
				if(value instanceof String){
					str += value+", ";
				}else if(value instanceof Object[]){
					Object[] array = (Object[])value;
					for(Object a : array){
						if(a instanceof String){
							str += a+", ";
						}
					}
				}
			}
		}
		
		str += "\""+parameter+"\"";
		
		str += "})";
		
		CompilationUnitChange change = new CompilationUnitChange("", compilationUnit);
		
		ReplaceEdit edit = new ReplaceEdit(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength(), str);
		change.setEdit(edit);
		
		return change;
	}

	private CompilationUnitChange addAnnotation(String name, ICompilationUnit compilationUnit, IJavaElement element, ASTNode node) throws JavaModelException{
		if(!(element instanceof ISourceReference))
			return null;
		
		ISourceReference workingCopySourceReference = (ISourceReference) element;
		
		IBuffer buffer = compilationUnit.getBuffer();
		
		int position = workingCopySourceReference.getSourceRange().getOffset();
		
		if(node != null){
			position = node.getStartPosition();
		}
		
		String str = AT+name;
		
		if(!(workingCopySourceReference instanceof ILocalVariable)){
			
			str += compilationUnit.findRecommendedLineSeparator();
			
			int index = position;
			while(index >= 0){
				char c = buffer.getChar(index);
				if(c == '\r' || c == '\n')
					break;
				index--;
			}
			index++;
			if(index != position){
				String spaces = buffer.getText(index, position-index);
				str += spaces;
			}
			
		}else{
			str += SPACE;
		}
		
		CompilationUnitChange change = new CompilationUnitChange("", compilationUnit);
		
		InsertEdit edit = new InsertEdit(position, str);
		
		change.setEdit(edit);
		
		return change;
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
