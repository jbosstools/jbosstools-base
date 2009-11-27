/*******************************************************************************
 * Copyright (c) 2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.common.el.ui.refactoring;

import java.io.IOException;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.ui.text.FastJavaPartitionScanner;
import org.eclipse.jdt.ui.text.IJavaPartitions;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.internal.services.IWorkbenchLocationService;
import org.eclipse.ui.menus.AbstractContributionFactory;
import org.eclipse.ui.menus.IContributionRoot;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.services.IServiceLocator;
import org.eclipse.wst.sse.core.StructuredModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IModelManager;
import org.eclipse.wst.sse.core.internal.provisional.IStructuredModel;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocumentRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.ITextRegionList;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMDocument;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMModel;
import org.eclipse.wst.xml.core.internal.provisional.document.IDOMNode;
import org.eclipse.wst.xml.core.internal.regions.DOMRegionContext;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.refactoring.RenameELVariableProcessor;
import org.jboss.tools.common.el.core.refactoring.RenameELVariableRefactoring;
import org.jboss.tools.common.el.ui.ElUIMessages;
import org.jboss.tools.common.el.ui.ElUiPlugin;
import org.jboss.tools.common.model.ui.editor.EditorPartWrapper;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.propertieseditor.PropertiesCompoundEditor;
import org.jboss.tools.common.util.FileUtil;
import org.jboss.tools.jst.web.ui.editors.WebCompoundEditor;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * @author Daniel Azarov
 */
public class ELRefactorContributionFactory extends AbstractContributionFactory {
	//private static final String ANNOTATION_NAME = "org.jboss.seam.annotations.Name"; //$NON-NLS-1$
	private static final String JAVA_EXT = "java"; //$NON-NLS-1$
	private static final String XML_EXT = "xml"; //$NON-NLS-1$
	private static final String XHTML_EXT = "xhtml"; //$NON-NLS-1$
	private static final String JSP_EXT = "jsp"; //$NON-NLS-1$
	private static final String PROPERTIES_EXT = "properties"; //$NON-NLS-1$
	private static final String GET = "get"; //$NON-NLS-1$
	private static final String SET = "set"; //$NON-NLS-1$
	private static final String IS = "is"; //$NON-NLS-1$
	
	static private String selectedText;
	static private IFile editorFile;
	private String fileContent;
	private IEditorPart editor;
	private Shell shell;
	
	public ELRefactorContributionFactory(){
		super("","");
	}
	
	public ELRefactorContributionFactory(String location, String namespace){
		super(location, namespace);
	}

	@Override
	public void createContributionItems(IServiceLocator serviceLocator,
			IContributionRoot additions) {
		
		if(serviceLocator.hasService(IWorkbenchLocationService.class)){
			IWorkbenchLocationService service = (IWorkbenchLocationService)serviceLocator.getService(IWorkbenchLocationService.class);
			editor = service.getWorkbenchWindow().getActivePage().getActiveEditor();
			shell = service.getWorkbench().getActiveWorkbenchWindow().getShell();
			
			if(!(editor.getEditorInput() instanceof FileEditorInput))
				return;
			
			FileEditorInput input = (FileEditorInput)editor.getEditorInput();
			
			editorFile = input.getFile();
			String ext = editorFile.getFileExtension();
			
			if (!JAVA_EXT.equalsIgnoreCase(ext)
					&& !XML_EXT.equalsIgnoreCase(ext)
					&& !XHTML_EXT.equalsIgnoreCase(ext)
					&& !JSP_EXT.equalsIgnoreCase(ext)
					&& !PROPERTIES_EXT.equalsIgnoreCase(ext))
				return;
			
			MenuManager mm = new MenuManager(ElUIMessages.REFACTOR_CONTRIBUTOR_MAIN_MENU);
			mm.setVisible(true);
			
			boolean separatorIsAdded = false;
			
			ISelection sel = editor.getEditorSite().getSelectionProvider().getSelection();
			
			if(sel == null || sel.isEmpty())
				return;
			
			if(sel instanceof StructuredSelection){
				if(editor instanceof PropertiesCompoundEditor){
					sel = ((PropertiesCompoundEditor)editor).getActiveEditor().getSite().getSelectionProvider().getSelection();
				}else if(editor instanceof EditorPartWrapper){
					EditorPartWrapper wrapperEditor = (EditorPartWrapper)editor;
					if(wrapperEditor.getEditor() instanceof WebCompoundEditor){
						WebCompoundEditor xmlEditor = (WebCompoundEditor)wrapperEditor.getEditor();
						sel = xmlEditor.getActiveEditor().getSite().getSelectionProvider().getSelection();
					}
				}else if(editor instanceof WebCompoundEditor)
					sel = ((WebCompoundEditor)editor).getActiveEditor().getSite().getSelectionProvider().getSelection();
			}

			if(sel instanceof TextSelection){
				TextSelection selection = (TextSelection)sel;

				selectedText = selection.getText();

				try {
					fileContent = FileUtil.readStream(editorFile);
				} catch (CoreException e) {
					ElUiPlugin.getDefault().logError(e);
				}

				boolean status = false;

				if(JAVA_EXT.equalsIgnoreCase(ext)){
					status = checkContextVariableInJava(editorFile, fileContent, selection);
				} else if(XML_EXT.equalsIgnoreCase(ext) || XHTML_EXT.equalsIgnoreCase(ext) || JSP_EXT.equalsIgnoreCase(ext))
					status = checkContextVariableInDOM(editorFile, fileContent, selection);
				else if(PROPERTIES_EXT.equalsIgnoreCase(ext))
					status = checkContextVariableInProperties(editorFile, fileContent, selection);

				if(status){
					mm.add(new RenameELVariableAction());

					if(!separatorIsAdded)
						additions.addContributionItem(new Separator(), null);
					additions.addContributionItem(mm, null);
				}
			}
		}
	}
	
	private boolean checkContextVariableInJava(IFile file, String content, TextSelection selection){
		try {
			FastJavaPartitionScanner scaner = new FastJavaPartitionScanner();
			Document document = new Document(content);
			scaner.setRange(document, 0, document.getLength());
			IToken token = scaner.nextToken();
			while(token!=null && token!=Token.EOF) {
				if(IJavaPartitions.JAVA_STRING.equals(token.getData())) {
					int length = scaner.getTokenLength();
					int offset = scaner.getTokenOffset();
					if(offset <= selection.getOffset() && (offset+length) >= (selection.getOffset()+selection.getLength())){
						String value = document.get(offset, length);
						if(value.indexOf('{')>-1) {
							return scanString(file, value, offset, selection);
						}
					}
				}
				token = scaner.nextToken();
			}
		} catch (BadLocationException e) {
			ElUiPlugin.getDefault().logError(e);
		}
		return false;
	}
	
	private boolean scanString(IFile file, String string, int offset, TextSelection selection) {
		int startEl = string.indexOf("#{"); //$NON-NLS-1$
		if(startEl>-1) {
			ELParser parser = ELParserUtil.getJbossFactory().createParser();
			ELModel model = parser.parse(string);
			for (ELInstance instance : model.getInstances()) {
				for(ELInvocationExpression ie : instance.getExpression().getInvocations()){
					ELPropertyInvocation pi = findELVariable(ie);
					if(pi != null){
						if(offset+pi.getStartPosition() == selection.getOffset() && pi.getLength() == selection.getLength())
							return true;
					}
				}
			}
		}
		return false;
	}
	
	private ELPropertyInvocation findELVariable(ELInvocationExpression invocationExpression){
		ELInvocationExpression invExp = invocationExpression;
		while(invExp != null){
			if(invExp instanceof ELPropertyInvocation){
				if(((ELPropertyInvocation)invExp).getQualifiedName() != null && ((ELPropertyInvocation)invExp).getQualifiedName().equals(selectedText))
					return (ELPropertyInvocation)invExp;
				else
					invExp = invExp.getLeft();
				
			}else{
				invExp = invExp.getLeft();
			}
		}
		return null;
	}
	
	private boolean checkContextVariableInDOM(IFile file, String content, TextSelection selection){
		IModelManager manager = StructuredModelManager.getModelManager();
		if(manager == null) {
			return false;
		}
		IStructuredModel model = null;		
		try {
			model = manager.getModelForRead(file);
			if (model instanceof IDOMModel) {
				IDOMModel domModel = (IDOMModel) model;
				IDOMDocument document = domModel.getDocument();
				return scanChildNodes(file, document, selection);
			}
		} catch (CoreException e) {
			ElUiPlugin.getDefault().logError(e);
        } catch (IOException e) {
        	ElUiPlugin.getDefault().logError(e);
		} finally {
			if (model != null) {
				model.releaseFromRead();
			}
		}
		return false;
	}
	
	private boolean scanChildNodes(IFile file, Node parent, TextSelection selection) {
		boolean status = false;
		NodeList children = parent.getChildNodes();
		for(int i=0; i<children.getLength(); i++) {
			Node curentValidatedNode = children.item(i);
			if(Node.ELEMENT_NODE == curentValidatedNode.getNodeType()) {
				status = scanNodeContent(file, ((IDOMNode)curentValidatedNode).getFirstStructuredDocumentRegion(), DOMRegionContext.XML_TAG_ATTRIBUTE_VALUE, selection);
				if(status)
					return status;
			} else if(Node.TEXT_NODE == curentValidatedNode.getNodeType()) {
				status = scanNodeContent(file, ((IDOMNode)curentValidatedNode).getFirstStructuredDocumentRegion(), DOMRegionContext.XML_CONTENT, selection);
				if(status)
					return status;
			}
			status = scanChildNodes(file, curentValidatedNode, selection);
			if(status)
				return status;
		}
		return false;
	}

	private boolean scanNodeContent(IFile file, IStructuredDocumentRegion node, String regionType, TextSelection selection) {
		boolean status = false;
		ITextRegionList regions = node.getRegions();
		for(int i=0; i<regions.size(); i++) {
			ITextRegion region = regions.get(i);
			if(region.getType() == regionType) {
				String text = node.getFullText(region);
				if(text.indexOf("{")>-1) { //$NON-NLS-1$
					int offset = node.getStartOffset() + region.getStart();
					status = scanString(file, text, offset, selection);
					if(status)
						return status;
				}
			}
		}
		return false;
	}

	private boolean checkContextVariableInProperties(IFile file, String content, TextSelection selection){
		return scanString(file, content, 0, selection);
	}
	
	private String getPropertyName(IMethod method){
		String name = method.getElementName();
		
		if(name.startsWith(GET) || name.startsWith(SET))
			return name.substring(3).toLowerCase();
		
		if(name.startsWith(IS))
			return name.substring(2).toLowerCase();
		
		return name.toLowerCase();
	}
	
	private static void saveAndBuild(){
		if(!ElUiPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getActivePage().saveAllEditors(true))
			return;
		
		try {
			Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD, null);
		} catch (InterruptedException e) {
			// do nothing
		}
	}
	
	public static void invokeRenameELVariableWizard(String oldName, Shell activeShell) {
		saveAndBuild();
		
		RenameELVariableProcessor processor = new RenameELVariableProcessor(editorFile, selectedText);
		RenameELVariableRefactoring refactoring = new RenameELVariableRefactoring(processor);
		RenameELVariableWizard wizard = new RenameELVariableWizard(refactoring, editorFile);
		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(wizard);
		try {
			String titleForFailedChecks = ElUIMessages.EL_REFACTOR_RENAME_HANDLER_ERROR;
			op.run(activeShell, titleForFailedChecks);
		} catch (final InterruptedException irex) {
			// operation was canceled
		}
	}
	
	class RenameELVariableAction extends Action{
		public RenameELVariableAction(){
			super(ElUIMessages.REFACTOR_CONTRIBUTOR_RENAME_EL_VARIABLE);
		}
		public void run(){
			saveAndBuild();
			
			invokeRenameELVariableWizard(selectedText, shell);
		}
	}
	
}
