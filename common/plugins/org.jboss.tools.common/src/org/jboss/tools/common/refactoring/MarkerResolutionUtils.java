/*******************************************************************************
 * Copyright (c) 2010 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.refactoring;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.Flags;
import org.eclipse.jdt.core.IAnnotatable;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IBuffer;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IImportContainer;
import org.eclipse.jdt.core.IImportDeclaration;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.ILocalVariable;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.IPackageDeclaration;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.ITypeParameter;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.BodyDeclaration;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.internal.core.JavaElement;
import org.eclipse.jdt.internal.corext.util.CodeFormatterUtil;
import org.eclipse.jpt.common.core.internal.utility.jdt.ASTTools;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.TextChange;
import org.eclipse.ltk.core.refactoring.TextEditBasedChange;
import org.eclipse.text.edits.DeleteEdit;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;
import org.eclipse.text.edits.TextEdit;
import org.jboss.tools.common.util.EclipseJavaUtil;

public class MarkerResolutionUtils {
	public static final String DOT = ".";  //$NON-NLS-1$
	public static final String DOTS = "...";  //$NON-NLS-1$
	public static final String COMMA = ",";  //$NON-NLS-1$
	public static final String SEMICOLON = ";";  //$NON-NLS-1$
	public static final String SPACE = " ";  //$NON-NLS-1$
	public static final String AT = "@";  //$NON-NLS-1$
	public static final String IMPLEMENTS = "implements";  //$NON-NLS-1$
	public static final String IMPORT = "import";  //$NON-NLS-1$
	public static final String STATIC = "static";  //$NON-NLS-1$
	public static final String EXTENDS = "extends";  //$NON-NLS-1$
	public static final String OPEN_BRACE = "{"; //$NON-NLS-1$
	public static final String CLOSE_BRACE = "}"; //$NON-NLS-1$
	public static final String OPEN_BOLD = "<b>"; //$NON-NLS-1$
	public static final String CLOSE_BOLD = "</b>"; //$NON-NLS-1$
	public static final String OPEN_DEL = "<del>"; //$NON-NLS-1$
	public static final String CLOSE_DEL = "</del>"; //$NON-NLS-1$
	public static final String NEW_LINE = "\n"; //$NON-NLS-1$
	public static final String LINE_BREAK = "<br>"; //$NON-NLS-1$
	public static final String PUBLIC = "public";  //$NON-NLS-1$
	public static final String PRIVATE = "private";  //$NON-NLS-1$
	public static final String PROTECTED = "protected";  //$NON-NLS-1$
	
	public static final char C_SPACE = ' ';  //$NON-NLS-1$
	public static final char C_TAB = '\t';
	public static final char C_CARRIAGE_RETURN = '\r';
	public static final char C_NEW_LINE = '\n';
	
	private static final int NUMBER_OF_STRINGS = 3;

	public static final HashSet<String> primitives = new HashSet<String>();
	static{
		primitives.add("void");  //$NON-NLS-1$
		primitives.add("int");  //$NON-NLS-1$
		primitives.add("java.lang.Integer");  //$NON-NLS-1$
		primitives.add("char");  //$NON-NLS-1$
		primitives.add("java.lang.Character");  //$NON-NLS-1$
		primitives.add("boolean");  //$NON-NLS-1$
		primitives.add("java.lang.Boolean");  //$NON-NLS-1$
		primitives.add("short");  //$NON-NLS-1$
		primitives.add("java.lang.Short");  //$NON-NLS-1$
		primitives.add("long");  //$NON-NLS-1$
		primitives.add("java.lang.Long");  //$NON-NLS-1$
		primitives.add("float");  //$NON-NLS-1$
		primitives.add("java.lang.Float");  //$NON-NLS-1$
		primitives.add("double");  //$NON-NLS-1$
		primitives.add("java.lang.Double");  //$NON-NLS-1$
		primitives.add("byte");  //$NON-NLS-1$
		primitives.add("java.lang.Byte");  //$NON-NLS-1$
		primitives.add("java.lang.String");  //$NON-NLS-1$
		primitives.add("java.lang.SuppressWarnings");  //$NON-NLS-1$
	}
	/**
	 * 
	 * @param qualifiedName
	 * @param compilationUnit
	 * @return true if there is import in compilation unit with the same short name
	 * @throws JavaModelException
	 */
	public static boolean addImport(String qualifiedName, ICompilationUnit compilationUnit) throws JavaModelException{
		return addImport(qualifiedName, compilationUnit, false, null);
	}
	
	public static boolean addImport(String qualifiedName, ICompilationUnit compilationUnit, MultiTextEdit rootEdit) throws JavaModelException{
		return addImport(qualifiedName, compilationUnit, false, rootEdit);
	}
	
	/**
	 * 
	 * @param qualifiedName
	 * @param compilationUnit
	 * @param staticFlag
	 * @return true if there is import in compilation unit with the same short name
	 * @throws JavaModelException
	 */
	public static boolean addImport(String qualifiedName, ICompilationUnit compilationUnit, boolean staticFlag) throws JavaModelException{
		return addImport(qualifiedName, compilationUnit, staticFlag, null);
	}
	
	private static int findPositionForImport(ICompilationUnit compilationUnit) throws JavaModelException{
		if(compilationUnit.getImportContainer().exists()){
			return compilationUnit.getImportContainer().getSourceRange().getOffset()+compilationUnit.getImportContainer().getSourceRange().getLength();
		}else{
			IPackageDeclaration[] packageDeclarations = compilationUnit.getPackageDeclarations();
			if(packageDeclarations.length == 0){
				return 0;
			}
			int position = 0;
			for(IPackageDeclaration declaration : packageDeclarations){
				position = declaration.getSourceRange().getOffset()+declaration.getSourceRange().getLength();
			}
			return position;
		}
	}
	
	private static boolean isDuplicate(MultiTextEdit rootEdit, String text){
		for(TextEdit edit : rootEdit.getChildren()){
			if(edit instanceof InsertEdit && ((InsertEdit) edit).getText().equals(text))
				return true;
		}
		return false;
	}

	public static boolean addImport(String qualifiedName, ICompilationUnit compilationUnit, boolean staticFlag, MultiTextEdit rootEdit) throws JavaModelException{
		if(primitives.contains(qualifiedName) || qualifiedName.indexOf(DOT) < 0)
			return false;
		
		if(qualifiedName != null){
			String shortName = getShortName(qualifiedName);
			
			IPackageDeclaration[] packages = compilationUnit.getPackageDeclarations();
			
			// local classes do not need to be imported
			String typePackage = qualifiedName.substring(0,qualifiedName.lastIndexOf(DOT));
			
			for(IPackageDeclaration packageDeclaration : packages){
				if(packageDeclaration.getElementName().equals(typePackage))
					return false;
			}
			
			for(IPackageDeclaration packageDeclaration : packages){
				IType type = compilationUnit.getJavaProject().findType(packageDeclaration.getElementName()+DOT+shortName);
				if(type != null && type.exists())
					return true;
			}
		
			IImportDeclaration[] importDeclarations = compilationUnit.getImports(); 
			
			for(IImportDeclaration importDeclaration : importDeclarations){
				String importName = importDeclaration.getElementName();
				String elementShort = getShortName(importName);
				if(importDeclaration.isOnDemand()){
					int importLastDot = importName.lastIndexOf(DOT);
					if(importLastDot == -1) return false; // invalid import declaration
					int elementLastDot = qualifiedName.lastIndexOf(DOT);
					if(elementLastDot == -1) return false; // invalid import declaration
					
					if(qualifiedName.substring(0, elementLastDot).equals(importName.substring(0, importLastDot)))
						return false;
				}
				
				if(importName.equals(qualifiedName))
					return false;
				if(elementShort.equals(shortName))
					return true;
				
			}
			if(rootEdit == null){
				if(staticFlag){
					compilationUnit.createImport(qualifiedName, null, Flags.AccStatic, new NullProgressMonitor());
				}else{
					compilationUnit.createImport(qualifiedName, null, new NullProgressMonitor());
				}
			}else{
				String staticStr = "";
				if(staticFlag){
					staticStr = STATIC+SPACE;
				}
				String text = compilationUnit.findRecommendedLineSeparator()+IMPORT+SPACE+staticStr+qualifiedName+SEMICOLON;
				if(!isDuplicate(rootEdit, text)){
					int importPosition = findPositionForImport(compilationUnit);
					TextEdit edit = new InsertEdit(importPosition, text);
					rootEdit.addChild(edit);
				}
			}
		}
		return false;
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
	
	public static String getPackageName(String qualifiedName){
		int lastDot = qualifiedName.lastIndexOf(DOT);
		String name;
		if(lastDot < 0)
			name = "";
		else
			name = qualifiedName.substring(0, lastDot);
		return name;
	}
	
	public static String[] getShortNames(String[] qualifiedNames){
		String[] shortNames = new String[qualifiedNames.length];
		for(int i = 0; i < qualifiedNames.length; i++){
			shortNames[i] = getShortName(qualifiedNames[i]);
		}
		return shortNames;
	}
	
	public static String getTotalList(String[] names){
		String list = "";
		for(int i = 0; i < names.length; i++){
			if(i != 0)
				list += ", ";
			list += names[i];
		}
		return list;
	}
	
	public static void addMethod(List<String> lines, ICompilationUnit compilationUnit, IType type, MultiTextEdit rootEdit) throws JavaModelException{
		IType workingCopyType = findWorkingCopy(compilationUnit, type);
		if(workingCopyType == null){
			return;
		}
		IBuffer buffer = compilationUnit.getBuffer();
		String lineSeparator = compilationUnit.findRecommendedLineSeparator();
		
		int position = workingCopyType.getSourceRange().getOffset()+workingCopyType.getSource().lastIndexOf("}");
		if(position > 0){
			String spaces = getLeadingSpacesToInsert(position, buffer);
			
			int indentWidth = CodeFormatterUtil.getIndentWidth(compilationUnit.getJavaProject());
			for(int i = 0; i < indentWidth; i++){
				spaces += SPACE;
			}
			
			String text = lineSeparator;
			for(String line : lines){
				text += spaces;
				text += line;
				text += lineSeparator;
			}
			
			if(rootEdit != null){
				TextEdit edit = new InsertEdit(position, text);
				rootEdit.addChild(edit);
			}else{
				buffer.replace(position, 0, text);
				
				synchronized(compilationUnit) {
					compilationUnit.reconcile(ICompilationUnit.NO_AST, true, null, null);
				}
			}
		}
	}

	public static void addAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element) throws JavaModelException{
		addAnnotation(qualifiedName, compilationUnit, element, "");
	}
	
	public static void updateAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element, String params) throws JavaModelException{
		updateAnnotation(qualifiedName, compilationUnit, element, params, null);
	}
	
	public static void updateAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element, String params, MultiTextEdit rootEdit) throws JavaModelException{
		IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, element);
		if(workingCopyElement == null){
			return;
		}
		
		if(!(workingCopyElement instanceof IMember))
			return;
		
		IMember workingCopyMember = (IMember) workingCopyElement;
		
		IAnnotation annotation = findAnnotation(workingCopyMember, qualifiedName);
		if(annotation == null || !annotation.exists())
			return;
		
		boolean duplicateShortName = addImport(qualifiedName, compilationUnit, null);
		
		IBuffer buffer = compilationUnit.getBuffer();
		String shortName = getShortName(qualifiedName);
		
		if(duplicateShortName)
			shortName = qualifiedName;
		
		String newValue = AT+shortName+params;
		
		if(!annotation.getSource().equals(newValue)){
			if(rootEdit != null){
				TextEdit edit = new ReplaceEdit(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength(), newValue);
				rootEdit.addChild(edit);
			}else{
				buffer.replace(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength(), newValue);
				
				synchronized(compilationUnit) {
					compilationUnit.reconcile(ICompilationUnit.NO_AST, true, null, null);
				}
			}
		}
		
	}

	public static void addAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element, String params) throws JavaModelException{
		addAnnotation(qualifiedName, compilationUnit, element, params, null);
	}
	
	public static void addAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element, String params, MultiTextEdit rootEdit) throws JavaModelException{
		IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, element);
		if(workingCopyElement == null){
			return;
		}
		
		if(!(workingCopyElement instanceof IMember))
			return;
		
		IMember workingCopyMember = (IMember) workingCopyElement;
		
		IAnnotation annotation = findAnnotation(workingCopyMember, qualifiedName);
		if(annotation != null && annotation.exists())
			return;
		
		CompilationUnit cuNode = ASTTools.buildASTRoot(compilationUnit);
		
		ASTNode elementNode = null;
		if(workingCopyElement instanceof JavaElement){
			elementNode = ((JavaElement) workingCopyElement).findNode(cuNode);
		}
		
		boolean duplicateShortName = addImport(qualifiedName, compilationUnit, rootEdit);
		
		IBuffer buffer = compilationUnit.getBuffer();
		String shortName = getShortName(qualifiedName);
		
		if(duplicateShortName)
			shortName = qualifiedName;
		
		String str = AT+shortName+params;
		
		int position = workingCopyMember.getSourceRange().getOffset();
		
		if(elementNode != null){
			position = elementNode.getStartPosition();
			if(elementNode instanceof BodyDeclaration && ((BodyDeclaration)elementNode).getJavadoc() != null){
				position += ((BodyDeclaration)elementNode).getJavadoc().getLength();
				char c = buffer.getChar(position);
				while((c == C_CARRIAGE_RETURN || c == C_NEW_LINE) && position < buffer.getLength()-2 ){
					position++;
					c = buffer.getChar(position);
				}
			}
			while(position < buffer.getLength()-1){
				char c = buffer.getChar(position);
				if(c != C_CARRIAGE_RETURN && c != C_NEW_LINE && c != C_SPACE && c != C_TAB){
					break;
				}
				position++;
			}
		}
		
		if(!(workingCopyMember instanceof ILocalVariable)){
			
			str += compilationUnit.findRecommendedLineSeparator();
			
			str += getLeadingSpacesToInsert(position, buffer);
			
		}else{
			str += SPACE;
		}
		
		if(rootEdit != null){
			TextEdit edit = new InsertEdit(position, str);
			rootEdit.addChild(edit);
		}else{
			buffer.replace(position, 0, str);
			
			synchronized(compilationUnit) {
				compilationUnit.reconcile(ICompilationUnit.NO_AST, true, null, null);
			}
		}
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
	}
	
	public static IAnnotation findAnnotation(IJavaElement element, String qualifiedName){
		if(element instanceof IAnnotatable){
			String name = getShortName(qualifiedName);
			IAnnotation annotation = ((IAnnotatable)element).getAnnotation(qualifiedName);
			if (!annotation.exists()) {
				annotation = ((IAnnotatable)element).getAnnotation(name);
			} else {
				return annotation;
			}
			if(annotation.exists()) {
				IType type=null;
				if(element instanceof IType){
					type = (IType)element;
				}else if(element instanceof IMember){
					type = ((IMember)element).getDeclaringType();
				}else if(element instanceof ITypeParameter){
					type = ((ITypeParameter)element).getDeclaringMember().getDeclaringType();
				}else if(element instanceof ILocalVariable){
					type = ((ILocalVariable)element).getDeclaringMember().getDeclaringType();
				}
				if (type != null && annotation != null && qualifiedName.equals(EclipseJavaUtil.resolveType(type, name))) {
					return annotation;
				}
			}
		}
		return null;
	}
	
	public static void addInterfaceToClass(ICompilationUnit compilationUnit, IType type, String qualifiedName, MultiTextEdit rootEdit) throws JavaModelException{
		String shortName = getShortName(qualifiedName);
		
		IType[] types = compilationUnit.getTypes();
		IType workingType = null;
		for(IType t : types){
			if(t.getElementName().equals(type.getElementName())){
				workingType = t;
				break;
			}
		}
		
		if(workingType != null){
			addImport(qualifiedName, compilationUnit, rootEdit);
			
			IBuffer buffer = compilationUnit.getBuffer();
			
			String text = buffer.getText(workingType.getSourceRange().getOffset(), workingType.getSourceRange().getLength());
			
			int namePosition = text.indexOf(workingType.getElementName());
			if(namePosition >= 0){
				int implementsPosition = text.indexOf(IMPLEMENTS,namePosition);
				if(implementsPosition > 0){
					if(rootEdit != null){
						TextEdit edit = new InsertEdit(workingType.getSourceRange().getOffset()+implementsPosition+IMPLEMENTS.length(), SPACE+shortName+COMMA);
						rootEdit.addChild(edit);
					}else{
						buffer.replace(workingType.getSourceRange().getOffset()+implementsPosition+IMPLEMENTS.length(),0,SPACE+shortName+COMMA);
					}
				}else{
					int extedsPosition = text.indexOf(EXTENDS,namePosition);
					if(extedsPosition > 0){
						int bracePosition = text.indexOf(OPEN_BRACE, extedsPosition);
						String str = IMPLEMENTS+SPACE+shortName+SPACE;
						if(!text.substring(bracePosition-1,bracePosition).equals(SPACE))
							str = SPACE+str;
						if(rootEdit != null){
							TextEdit edit = new InsertEdit(workingType.getSourceRange().getOffset()+bracePosition, str);
							rootEdit.addChild(edit);
						}else{
							buffer.replace(workingType.getSourceRange().getOffset()+bracePosition,0,str);
						}
					}else{
						if(rootEdit != null){
							TextEdit edit = new InsertEdit(workingType.getSourceRange().getOffset()+namePosition+workingType.getElementName().length(), SPACE+IMPLEMENTS+SPACE+shortName);
							rootEdit.addChild(edit);
						}else{
							buffer.replace(workingType.getSourceRange().getOffset()+namePosition+workingType.getElementName().length(),0,SPACE+IMPLEMENTS+SPACE+shortName);
						}
					}
				}
			}
		}
	}
	
	public static void deleteAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element) throws JavaModelException{
		deleteAnnotation(qualifiedName, compilationUnit, element, null);
	}
	
	public static void deleteAnnotation(String qualifiedName, ICompilationUnit compilationUnit, IJavaElement element, MultiTextEdit rootEdit) throws JavaModelException{
		IJavaElement workingCopyElement = findWorkingCopy(compilationUnit, element);
		if(workingCopyElement == null){
			return;
		}
		
		IAnnotation annotation = findAnnotation(workingCopyElement, qualifiedName);
		if(annotation != null){
			IBuffer buffer = compilationUnit.getBuffer();
			
			int numberOfSpaces = getNumberOfSpacesToDelete(annotation.getSourceRange().getOffset() + annotation.getSourceRange().getLength(), buffer);
			
			// delete annotation
			if(rootEdit != null){
				TextEdit edit = new DeleteEdit(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength()+numberOfSpaces);
				rootEdit.addChild(edit);
			}else{
				buffer.replace(annotation.getSourceRange().getOffset(), annotation.getSourceRange().getLength()+numberOfSpaces, "");
			}
			
			// check and delete import
			deleteImportForAnnotation(qualifiedName, annotation, compilationUnit, buffer, rootEdit);
		}
	}
	
	private static int getNumberOfSpacesToDelete(int startPosition, IBuffer buffer){
		int position = startPosition;
		int numberOfSpaces = 0;
		if(position < buffer.getLength()-1){
			char c = buffer.getChar(position);
			while((c == C_SPACE || c == C_TAB || c == C_NEW_LINE || c == C_CARRIAGE_RETURN) && position < buffer.getLength()-1){
				numberOfSpaces++;
				position++;
				c = buffer.getChar(position);
			}
		}
		return numberOfSpaces;
	}
	
	private static String getLeadingSpacesToInsert(int startPosition, IBuffer buffer){
		int position = startPosition;
		while(position >= 0){
			char c = buffer.getChar(position);
			if(c == C_CARRIAGE_RETURN || c == C_NEW_LINE)
				break;
			position--;
		}
		position++;
		if(position != startPosition){
			return buffer.getText(position, startPosition-position);
		}
		return "";
	}
	
	public static void deleteImportForAnnotation(String qualifiedName, IAnnotation annotation, ICompilationUnit compilationUnit, IBuffer buffer, MultiTextEdit rootEdit) throws JavaModelException{
		IImportDeclaration importDeclaration = compilationUnit.getImport(qualifiedName);
		IImportContainer importContainer = compilationUnit.getImportContainer();
		if(importDeclaration.exists() && importContainer.exists()){
			int importSize = importContainer.getSourceRange().getOffset()+importContainer.getSourceRange().getLength();
			
				if(rootEdit != null){
					int annotationStart = annotation.getSourceRange().getOffset();
					int annotationEnd = annotationStart+annotation.getSourceRange().getLength();
					String textBefore = buffer.getText(importSize, annotationStart-importSize);
					String textAfter = buffer.getText(annotationEnd, buffer.getLength()-annotationEnd);
					if(checkImport(textBefore, qualifiedName) && checkImport(textAfter, qualifiedName)){
						int numberOfSpaces = 0;
						if(!isLastImport(importContainer, importDeclaration)){
							numberOfSpaces = getNumberOfSpacesToDelete(importDeclaration.getSourceRange().getOffset() + importDeclaration.getSourceRange().getLength(), buffer);
						}

						TextEdit edit = new DeleteEdit(importDeclaration.getSourceRange().getOffset(), importDeclaration.getSourceRange().getLength()+numberOfSpaces);
						rootEdit.addChild(edit);
					}
				}else{
					String text = buffer.getText(importSize, buffer.getLength()-importSize);
					if(checkImport(text, qualifiedName)){
						importDeclaration.delete(false, new NullProgressMonitor());
					}
				}
		}
		
		if(rootEdit == null){
			synchronized(compilationUnit) {
				compilationUnit.reconcile(ICompilationUnit.NO_AST, true, null, null);
			}
		}
	}
	
	private static boolean isLastImport(IImportContainer importContainer, IImportDeclaration importDeclaration) throws JavaModelException{
		for(int index = 0; index < importContainer.getChildren().length; index++){
			IJavaElement child = importContainer.getChildren()[index];
			
			if(child.equals(importDeclaration) && index == importContainer.getChildren().length-1){
				return true;
			}
		}
		return false;
	}
	
	public static boolean checkImport(String text, String qualifiedName){
		String name = getShortName(qualifiedName);
		
		Pattern p = Pattern.compile(".*\\W"+name+"\\W.*",Pattern.DOTALL); //$NON-NLS-1$ //$NON-NLS-2$
		Matcher m = p.matcher(text);
		return !m.matches();
	}
	
	public static IMember getJavaMember(IJavaElement element){
		while(element != null){
			if(element instanceof IMember)
				return (IMember)element;
			element = element.getParent();
		}
		return null;
	}
	
	public static String getPreview(Change previewChange) throws CoreException{
		if(previewChange == null)
			return null;
		
		// CompositeChange
		// TextEditBasedChange
		// TextChange
		
		
		String preview = getPreviewContent(previewChange);
		//String current = previewChange.getCurrentContent(new NullProgressMonitor());
		
		TextEdit edit = getEdit(previewChange);
		
		if(preview != null && edit != null){
			EditSet editSet = new EditSet(edit);
			
			// select
			preview = editSet.select(preview);
			
			// cut
			preview = editSet.cut(preview);
			
			// format
			preview = preview.replaceAll(NEW_LINE, LINE_BREAK);
			
			return preview;
		}
		return "";
	}
	
	private static String getPreviewContent(Change change) throws CoreException{
		if(change instanceof TextEditBasedChange){
			return ((TextEditBasedChange) change).getPreviewContent(new NullProgressMonitor());
		}else if(change instanceof TextChange){
			return ((TextChange) change).getPreviewContent(new NullProgressMonitor());
		}else if(change instanceof CompositeChange){
			for(Change child : ((CompositeChange) change).getChildren()){
				String prev = getPreviewContent(child);
				if(prev != null){
					return prev;
				}
			}
		}
		return null;
	}

	private static TextEdit getEdit(Change change) throws CoreException{
		if(change instanceof BaseFileChange){
			return ((BaseFileChange) change).getEdit();
		}else if(change instanceof TextChange){
			return ((TextChange) change).getEdit();
		}else if(change instanceof CompositeChange){
			for(Change child : ((CompositeChange) change).getChildren()){
				TextEdit prev = getEdit(child);
				if(prev != null){
					return prev;
				}
			}
		}
		return null;
	}
	
	static class EditSet{
		private ArrayList<TextEdit> edits = new ArrayList<TextEdit>();
		private ArrayList<Region> regions = new ArrayList<Region>();
		
		public EditSet(TextEdit edit){
			addEdits(edit);
			sort();
		}
		
		private void addEdits(TextEdit edit){
			edits.add(edit);
			for(TextEdit child : edit.getChildren()){
				addEdits(child);
			}
		}
		
		private void sort(){
			if(edits.size() < 2){
				return;
			}
			ArrayList<TextEdit> sorted = new ArrayList<TextEdit>();
			int offset = 0;
			int distance = 0;
			int number = edits.size();
			for(int i = 0; i < number; i++){
				TextEdit edit = edits.get(0);
				distance = edit.getOffset()-offset;
				for(TextEdit e : edits){
					if((e.getOffset()-offset) < distance){
						distance = e.getOffset()-offset;
						edit = e;
					}
				}
				sorted.add(edit);
				edits.remove(edit);
				offset = edit.getOffset();
			}
			edits = sorted;
		}
		
		public String select(String preview){
			int delta = 0;
			for(TextEdit edit : edits){
				String text = null;
				int addings = 0;
				if(edit instanceof InsertEdit){
					text = ((InsertEdit) edit).getText();
					addings = text.length();
				}else if(edit instanceof ReplaceEdit){
					text = ((ReplaceEdit) edit).getText();
					addings = text.length()-edit.getLength();
				}else if(edit instanceof DeleteEdit){
					int offset = edit.getOffset()+delta;
					int length = edit.getLength();
					regions.add(new Region(offset, 0));
					delta -= length;
				}
				if(text != null){
					int offset = edit.getOffset()+delta;
					int length = text.length();
					regions.add(new Region(offset, length));
					
					// select
					String before = preview.substring(0, offset);
					String after = preview.substring(offset+length);
					preview = before+OPEN_BOLD+text+CLOSE_BOLD+after;
					
					delta += OPEN_BOLD.length()+CLOSE_BOLD.length()+addings;
				}
			}
			return preview;
		}
		
		public String cut(String preview){
			// process regions
			Region prevRegion = null;
			for(Region region : regions){
				
				int position = region.offset;
				int count = NUMBER_OF_STRINGS;
				int lowLimit = 0;
				if(prevRegion != null){
					lowLimit = prevRegion.offset+prevRegion.length;
				}
				if(position > preview.length()-1){
					position = preview.length()-1;
				}
				while(position >= lowLimit){
					char c = preview.charAt(position);
					if(c == C_NEW_LINE){
						count--;
						if(count == 0){
							position++;
							break;
						}
					}
					position--;
				}
				if(prevRegion != null && position <= prevRegion.offset+prevRegion.length){
					prevRegion.active = false;
					int shift = region.offset - prevRegion.offset;
					region.offset = prevRegion.offset;
					region.length += shift;
				}else{
					int shift = region.offset-position;
					region.offset = position;
					region.length += shift;
					
				}
				
				position = region.offset+region.length;
				count = NUMBER_OF_STRINGS;
				while(position < preview.length()-1){
					char c = preview.charAt(position);
					if(c == C_NEW_LINE){
						count--;
						if(count == 0){
							break;
						}
					}
					position++;
				}
				region.length = position - region.offset;
				prevRegion = region;
			}
			
			
			// cut
			StringBuffer buffer = new StringBuffer();
			int index = 0;
			for(Region region : regions){
				if(!region.active){
					continue;
				}
				if(region.offset > preview.length()-1){
					region.offset = preview.length()-1;
				}
				if(region.offset < 0){
					region.offset = 0;
				}
				if(region.offset + region.length > preview.length()-1){
					region.length = (preview.length()-1)-region.offset;
				}
				if(index == 0 && region.offset != 0){
					buffer.append(DOTS+NEW_LINE);
				}
				if(region.length > 0){
					buffer.append(preview.substring(region.offset, region.offset + region.length));
				}
				if((region.offset + region.length) < (preview.length()-1)){
					if(index == regions.size()-1){
						buffer.append(NEW_LINE+DOTS);
					}else{
						buffer.append(NEW_LINE+DOTS+NEW_LINE);
					}
				}
				index++;
			}
			return buffer.toString();
		}
	}
	
	static class Region{
		public int offset;
		public int length;
		public boolean active = true;
		
		public Region(int offset, int length){
			this.offset = offset;
			this.length = length;
		}
	}
}
