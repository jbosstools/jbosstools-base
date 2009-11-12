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
package org.jboss.tools.common.el.core.refactoring;

import java.io.IOException;
import java.util.List;
import java.util.StringTokenizer;

import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IMethod;
import org.eclipse.jdt.core.search.IJavaSearchScope;
import org.eclipse.jdt.internal.ui.text.FastJavaPartitionScanner;
import org.eclipse.jdt.ui.text.IJavaPartitions;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.Token;
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
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.resolver.ELCompletionEngine;
import org.jboss.tools.common.el.core.resolver.ELResolution;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELResolverFactoryManager;
import org.jboss.tools.common.el.core.resolver.ELSegment;
import org.jboss.tools.common.el.core.resolver.ElVarSearcher;
import org.jboss.tools.common.el.core.resolver.SimpleELContext;
import org.jboss.tools.common.el.core.resolver.Var;
import org.jboss.tools.common.model.util.EclipseResourceUtil;
import org.jboss.tools.common.util.FileUtil;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public abstract class RefactorSearcher {
	protected static final String JAVA_EXT = "java"; //$NON-NLS-1$
	protected static final String XML_EXT = "xml"; //$NON-NLS-1$
	protected static final String XHTML_EXT = "xhtml"; //$NON-NLS-1$
	protected static final String JSP_EXT = "jsp"; //$NON-NLS-1$
	protected static final String PROPERTIES_EXT = "properties"; //$NON-NLS-1$
	
	private static final String GET = "get"; //$NON-NLS-1$
	private static final String SET = "set"; //$NON-NLS-1$
	private static final String IS = "is"; //$NON-NLS-1$
	
	protected static final String SEAM_PROPERTIES_FILE = "seam.properties"; //$NON-NLS-1$
	
	protected IFile baseFile;
	protected String propertyName;
	protected IJavaElement javaElement;
	protected IJavaSearchScope searchScope;
	
	
	public RefactorSearcher(IFile baseFile, String propertyName){
		this.baseFile = baseFile;
		this.propertyName = propertyName;
	}
	
	public RefactorSearcher(IFile baseFile, String propertyName, IJavaElement javaElement){
		this(baseFile, propertyName);
		this.javaElement = javaElement;
	}
	
	public void setSearchScope(IJavaSearchScope searchScope){
		this.searchScope = searchScope;
	}

	public void findELReferences(){
		if(baseFile == null)
			return;
		
		//startStatistics();
		
		IProject[] projects = getProjects();
		for (IProject project : projects) {
			if(project == null) continue;
			
			if(!containsInSearchScope(project))
				continue;
			
			updateEnvironment(project);
			
			IJavaProject javaProject = EclipseResourceUtil.getJavaProject(project);
			
			// searching java, xml and property files in source folders
			if(javaProject != null){
				for(IResource resource : EclipseResourceUtil.getJavaSourceRoots(project)){
					if(resource instanceof IFolder)
						scanForJava((IFolder) resource);
					else if(resource instanceof IFile)
						scanForJava((IFile) resource);
				}
			}
			
			// searching jsp, xhtml and xml files in WebContent folders
			
			if(getViewFolder(project) != null)
				scan(getViewFolder(project));
			else
				scan(project);
		}
		//stopStatistic();
	}
	
	protected void updateEnvironment(IProject project){
		
	}
	
	protected abstract IProject[] getProjects();
	
	protected abstract IContainer getViewFolder(IProject project);
	
	private void scanForJava(IContainer container){
		try{
			for(IResource resource : container.members()){
				if(resource instanceof IFolder)
					scanForJava((IFolder) resource);
				else if(resource instanceof IFile)
					scanForJava((IFile) resource);
			}
		}catch(CoreException ex){
			ELCorePlugin.getDefault().logError(ex);
		}
	}

	private void scan(IContainer container){
		try{
			for(IResource resource : container.members()){
				if(resource instanceof IFolder)
					scan((IFolder) resource);
				else if(resource instanceof IFile)
					scan((IFile) resource);
			}
		}catch(CoreException ex){
			ELCorePlugin.getDefault().logError(ex);
		}
	}
	
	private void scanForJava(IFile file){
		if(isFileCorrect(file)) {
			String content = null;
			try {
				content = FileUtil.readStream(file);
				//collectStatistic(content.length());
			} catch (CoreException e) {
				ELCorePlugin.getDefault().logError(e);
			}
			if(content!= null) { 
				String ext = file.getFileExtension();
				if(JAVA_EXT.equalsIgnoreCase(ext)){
					scanJava(file, content);
				} else if(XML_EXT.equalsIgnoreCase(ext)) {
					scanDOM(file, content);
				} else if(PROPERTIES_EXT.equalsIgnoreCase(ext)) {
					scanProperties(file, content);
				}
			}
		}
	}

	private void scan(IFile file){
		if(isFileCorrect(file)) {
			String content = null;
			try {
				content = FileUtil.readStream(file);
				//collectStatistic(content.length());
			} catch (CoreException e) {
				ELCorePlugin.getDefault().logError(e);
			}
			if(content!=null) {
				String ext = file.getFileExtension();			
				if(XML_EXT.equalsIgnoreCase(ext) 
					|| XHTML_EXT.equalsIgnoreCase(ext) 
					|| JSP_EXT.equalsIgnoreCase(ext)) {
					scanDOM(file, content);
				}
			}
		}
	}
	
	private void scanJava(IFile file, String content){
		try {
			FastJavaPartitionScanner scaner = new FastJavaPartitionScanner();
			Document document = new Document(content);
			scaner.setRange(document, 0, document.getLength());
			IToken token = scaner.nextToken();
			while(token!=null && token!=Token.EOF) {
				if(IJavaPartitions.JAVA_STRING.equals(token.getData())) {
					int length = scaner.getTokenLength();
					int offset = scaner.getTokenOffset();
					String value = document.get(offset, length);
					if(value.indexOf('{')>-1) {
						scanString(file, value, offset);
					}
				}
				token = scaner.nextToken();
			}
		} catch (BadLocationException e) {
			ELCorePlugin.getDefault().logError(e);
		}
	}
	
	private void scanDOM(IFile file, String content){
		IModelManager manager = StructuredModelManager.getModelManager();
		if(manager != null) {
			IStructuredModel model = null;		
			try {
				model = manager.getModelForRead(file);
				if (model instanceof IDOMModel) {
					IDOMModel domModel = (IDOMModel) model;
					IDOMDocument document = domModel.getDocument();
					scanChildNodes(file, document);
				}
			} catch (CoreException e) {
				ELCorePlugin.getDefault().logError(e);
	        } catch (IOException e) {
	        	ELCorePlugin.getDefault().logError(e);
			} finally {
				if (model != null) {
					model.releaseFromRead();
				}
			}
		}
	}
	
	private void scanChildNodes(IFile file, Node parent) {
		NodeList children = parent.getChildNodes();
		for(int i=0; i<children.getLength(); i++) {
			Node curentValidatedNode = children.item(i);
			if(Node.ELEMENT_NODE == curentValidatedNode.getNodeType()) {
				scanNodeContent(file, ((IDOMNode)curentValidatedNode).getFirstStructuredDocumentRegion(), DOMRegionContext.XML_TAG_ATTRIBUTE_VALUE);
			} else if(Node.TEXT_NODE == curentValidatedNode.getNodeType()) {
				scanNodeContent(file, ((IDOMNode)curentValidatedNode).getFirstStructuredDocumentRegion(), DOMRegionContext.XML_CONTENT);
			}
			scanChildNodes(file, curentValidatedNode);
		}
	}

	private void scanNodeContent(IFile file, IStructuredDocumentRegion node, String regionType) {
		ITextRegionList regions = node.getRegions();
		for(int i=0; i<regions.size(); i++) {
			ITextRegion region = regions.get(i);
			if(region.getType() == regionType) {
				String text = node.getFullText(region);
				if(text.indexOf("{")>-1) { //$NON-NLS-1$
					int offset = node.getStartOffset() + region.getStart();
					scanString(file, text, offset);
				}
			}
		}
	}

	// looking for component references in EL
	private void scanString(IFile file, String string, int offset) {
		int startEl = string.indexOf("#{"); //$NON-NLS-1$
		if(startEl<0)
			startEl = string.indexOf("${"); //$NON-NLS-1$
		if(startEl>-1) {
			ELParser parser = ELParserUtil.getJbossFactory().createParser();
			ELModel model = parser.parse(string);
			for (ELInstance instance : model.getInstances()) {
				for(ELInvocationExpression ie : instance.getExpression().getInvocations()){
					ELInvocationExpression expression = findComponentReference(ie);
					if(expression != null){
						checkMatch(file, expression, offset+getOffset(expression), getLength(expression));
					}
				}
			}
		}
	}
	
	protected int getOffset(ELInvocationExpression expression){
		if(expression instanceof ELPropertyInvocation){
			ELPropertyInvocation pi = (ELPropertyInvocation)expression;
			
			if(pi.getName() != null)
				return pi.getName().getStart();
		}else if(expression instanceof ELMethodInvocation){
			ELMethodInvocation mi = (ELMethodInvocation)expression;
			
			if(mi.getName() != null)
				return mi.getName().getStart();
		}
		return 0;
	}
	
	private int getLength(ELInvocationExpression expression){
		if(expression instanceof ELPropertyInvocation){
			ELPropertyInvocation pi = (ELPropertyInvocation)expression;
			
			if(pi.getName() != null)
				return pi.getName().getLength();
		}else if(expression instanceof ELMethodInvocation){
			ELMethodInvocation mi = (ELMethodInvocation)expression;
			
			if(mi.getName() != null)
				return mi.getName().getLength();
		}
		return 0;
	}
	
	private void scanProperties(IFile file, String content){
		scanString(file, content, 0);
		
		if(!file.getName().equals(SEAM_PROPERTIES_FILE))
			return;
		
		StringTokenizer tokenizer = new StringTokenizer(content, "#= \t\r\n\f", true); //$NON-NLS-1$
		
		String lastToken = "\n"; //$NON-NLS-1$
		int offset = 0;
		boolean comment = false;
		boolean key = true;
		
		while(tokenizer.hasMoreTokens()){
			String token = tokenizer.nextToken("#= \t\r\n\f"); //$NON-NLS-1$
			if(token.equals("\r")) //$NON-NLS-1$
				token = "\n"; //$NON-NLS-1$
			
			if(token.equals("#") && lastToken.equals("\n")) //$NON-NLS-1$ //$NON-NLS-2$
				comment = true;
			else if(token.equals("\n") && comment) //$NON-NLS-1$
				comment = false;
			
			if(!comment){
				if(!token.equals("\n") && lastToken.equals("\n")) //$NON-NLS-1$ //$NON-NLS-2$
					key = true;
				else if(key && (token.equals("=") || token.equals(" "))) //$NON-NLS-1$ //$NON-NLS-2$
					key = false;
				
				if(key && token.startsWith(propertyName)){
					match(file, offset, token.length());
				}
			}
			
			lastToken = token;
			offset += token.length();
		}
	}
	
	protected ELInvocationExpression findComponentReference(ELInvocationExpression invocationExpression){
		return invocationExpression;
	}
	
	protected abstract boolean isFileCorrect(IFile file);
	
	protected abstract void match(IFile file, int offset, int length);
	
	protected void checkMatch(IFile file, ELExpression operand, int offset, int length){
		if(javaElement != null && operand != null)
			resolve(file, operand, offset-getOffset((ELInvocationExpression)operand));
		else
			match(file, offset, length);
	}
	
	// TODO: move to util class
	public static boolean isGetter(IMethod method) {
		String name = method.getElementName();
		int numberOfParameters = method.getNumberOfParameters();
		
		return (((name.startsWith(GET) && !name.equals(GET)) || name.startsWith(IS)) && numberOfParameters == 0);
	}

	// TODO: move to util class
	public static boolean isSetter(IMethod method) {
		String name = method.getElementName();
		int numberOfParameters = method.getNumberOfParameters();

		return ((name.startsWith(SET) && !name.equals(SET)) && numberOfParameters == 1);
	}
	
	// TODO: move to util class
	public static String getPropertyName(IMethod method, String methodName){
		if (isGetter(method) || isSetter(method)) {
			StringBuffer name = new StringBuffer(methodName);
			if(methodName.startsWith("i")) { //$NON-NLS-1$
				name.delete(0, 2);
			} else {
				name.delete(0, 3);
			}
			if(name.length()<2 || Character.isLowerCase(name.charAt(1))) {
				name.setCharAt(0, Character.toLowerCase(name.charAt(0)));
			}
			String propertyName = name.toString();
			return propertyName;
		}
		return methodName;
	}
	
	private boolean containsInSearchScope(IProject project){
		if(searchScope == null)
			return true;
		IPath[] paths = searchScope.enclosingProjectsAndJars();
		for(IPath path : paths){
			if(path.equals(project.getFullPath()))
				return true;
		}
		return false;
	}

	protected void resolve(IFile file, ELExpression operand, int offset) {
		ELResolver[] resolvers = ELResolverFactoryManager.getInstance()
				.getResolvers(file);

		for (ELResolver resolver : resolvers) {
			if (!(resolver instanceof ELCompletionEngine))
				continue;

			SimpleELContext context = new SimpleELContext();

			context.setResource(file);
			context.setElResolvers(resolvers);

			List<Var> vars = ElVarSearcher.findAllVars(context, offset,
					resolver);

			context.setVars(vars);

			ELResolution resolution = resolver.resolve(context, operand);

			List<ELSegment> segments = resolution.findSegmentsByJavaElement(javaElement);
			
			for(ELSegment segment : segments){
				match(file, offset+segment.getSourceReference().getStartPosition(), segment.getSourceReference().getLength());
			}
		}
	}
	// performance measure 
//	private int totalSize = 0;
//	private int filesNumber = 0;
//	private long startTime = 0;
//	private long stopTime = 0;
//	private long startMem = 0;
//	private long stopMem = 0;
//	
//	private boolean log = false;
//	
//	private void clearHistory(){
//		totalSize = 0;
//		filesNumber = 0;
//		startTime = 0;
//		stopTime = 0;
//		startMem = 0;
//		stopMem = 0;
//	}
//	
//	private void startStatistics(){
//		clearHistory();
//		startTime = System.currentTimeMillis();
//		startMem = Runtime.getRuntime().freeMemory();
//	}
//	
//	private void stopStatistic(){
//		stopTime = System.currentTimeMillis();
//		stopMem = Runtime.getRuntime().freeMemory();
//		printELSearchStatistics();
//	}
//	
//	private void collectStatistic(int fileSize){
//		filesNumber++;
//		totalSize += fileSize;
//	}
//	
//	private void printELSearchStatistics(){
//		if(log){
//			System.out.println("EL Search"); //$NON-NLS-1$
//			System.out.println("Total files number: "+getFilesNumber()); //$NON-NLS-1$
//			System.out.println("Total files size: "+getTotlalFilesSize()+" Mb"); //$NON-NLS-1$ $NON-NLS-2$
//			System.out.println("Memory usage size: "+getTotlalMemorySize()+" Mb"); //$NON-NLS-1$ $NON-NLS-2$
//			System.out.println("Free Memory size: "+getRestMemorySize()+" Mb"); //$NON-NLS-1$ $NON-NLS-2$
//			System.out.println("Total time: "+getTotalTime()+" sec"); //$NON-NLS-1$ $NON-NLS-2$
//		}
//	}
//	
//	private double getTotlalFilesSize(){
//		return (double)totalSize/(1024*1025);
//	}
//
//	private double getTotlalMemorySize(){
//		return (double)(startMem-stopMem)/(1024*1025);
//	}
//
//	private double getRestMemorySize(){
//		return (double)stopMem/(1024*1025);
//	}
//	
//	private int getFilesNumber(){
//		return filesNumber;
//	}
//	
//	private double getTotalTime(){
//		return (double)(stopTime - startTime)/1000;
//	}

}