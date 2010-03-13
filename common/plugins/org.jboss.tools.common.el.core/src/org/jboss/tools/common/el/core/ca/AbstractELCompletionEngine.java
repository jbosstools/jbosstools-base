/******************************************************************************* 
 * Copyright (c) 2007-2009 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core.ca;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.eclipse.core.resources.IFile;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IMember;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jdt.internal.ui.text.java.ProposalInfo;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.el.core.model.ELArgumentInvocation;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELObjectType;
import org.jboss.tools.common.el.core.model.ELPropertyInvocation;
import org.jboss.tools.common.el.core.model.ELUtil;
import org.jboss.tools.common.el.core.parser.ELParser;
import org.jboss.tools.common.el.core.parser.ELParserFactory;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.parser.LexicalToken;
import org.jboss.tools.common.el.core.resolver.ELCompletionEngine;
import org.jboss.tools.common.el.core.resolver.ELContext;
import org.jboss.tools.common.el.core.resolver.ELResolution;
import org.jboss.tools.common.el.core.resolver.ELResolutionImpl;
import org.jboss.tools.common.el.core.resolver.ELResolver;
import org.jboss.tools.common.el.core.resolver.ELSegment;
import org.jboss.tools.common.el.core.resolver.ELSegmentImpl;
import org.jboss.tools.common.el.core.resolver.ElVarSearcher;
import org.jboss.tools.common.el.core.resolver.IVariable;
import org.jboss.tools.common.el.core.resolver.JavaMemberELSegment;
import org.jboss.tools.common.el.core.resolver.JavaMemberELSegmentImpl;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector;
import org.jboss.tools.common.el.core.resolver.Var;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberInfo;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberPresentation;
import org.jboss.tools.common.el.internal.core.parser.token.JavaNameTokenDescription;
import org.jboss.tools.common.text.TextProposal;

public abstract class AbstractELCompletionEngine<V extends IVariable> implements ELResolver, ELCompletionEngine {

	public AbstractELCompletionEngine() {}

	public abstract Image getELProposalImage();

	protected abstract void log(Exception e);

	private static ELParserFactory defaultFactory = ELParserUtil.getJbossFactory();

	/* (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolver#getProposals(org.jboss.tools.common.el.core.resolver.ELContext, int)
	 */
	public List<TextProposal> getProposals(ELContext context, int offset) {
		ELReference ref = context.getELReference(offset);
		if(ref!=null) {
			for (ELExpression expresion : ref.getEl()) {
				if(ref.getStartPosition() + expresion.getStartPosition()<=offset && (ref.getStartPosition() + expresion.getEndPosition()>=offset)) {
					ELInvocationExpression ie = ELUtil.findExpression(expresion.getModel(), offset - ref.getStartPosition());
					String el = "#{"; //$NON-NLS-1$
					if(ie!=null) {
						el = el + ref.getSourceText().substring(ie.getStartPosition(), offset - ref.getStartPosition());
					}
					return getProposals(context, el, offset);
				}
			}
		}
		return Collections.emptyList();
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolver2#getProposals(org.jboss.tools.common.el.core.resolver.ELContext, java.lang.String)
	 */
	public List<TextProposal> getProposals(ELContext context, String el, int offset) {
		List<TextProposal> completions = new ArrayList<TextProposal>();

		List<Var> vars = new ArrayList<Var>();
		Var[] array = context.getVars(offset);
		for (int i = 0; i < array.length; i++) {
			vars.add(array[i]);
		}

		ELResolutionImpl resolution;
		try {
			resolution = resolveELOperand(context.getResource(), parseOperand(el), false, vars, new ElVarSearcher(context.getResource(), this));
			completions.addAll(resolution.getProposals());
		} catch (StringIndexOutOfBoundsException e) {
			log(e);
		} catch (BadLocationException e) {
			log(e);
		}
		
		return completions;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELResolver#resolve(org.jboss.tools.common.el.core.resolver.ELContext, org.jboss.tools.common.el.core.model.ELExpression)
	 */
	public ELResolution resolve(ELContext context, ELExpression operand, int offset) {
		List<Var> vars = new ArrayList<Var>();
		Var[] array = context.getVars(offset);
		for (int i = 0; i < array.length; i++) {
			vars.add(array[i]);
		}
		ELResolutionImpl resolution = null;
		try {
			resolution = resolveELOperand(context.getResource(), operand, true, vars, new ElVarSearcher(context.getResource(), this));
			if(resolution != null)
				resolution.setContext(context);
		} catch (StringIndexOutOfBoundsException e) {
			log(e);
		} catch (BadLocationException e) {
			log(e);
		}
		return resolution;
	}

	/**
	 * Resolves EL Operand
	 * 
	 * @param operand
	 * @param context
	 * @param returnEqualedVariablesOnly
	 * @return
	 */
	public ELResolution resolveELOperand(ELExpression operand, ELContext context, boolean returnEqualedVariablesOnly, int offset) {
		List<Var> vars = new ArrayList<Var>();
		Var[] array = context.getVars(offset);
		for (int i = 0; i < array.length; i++) {
			vars.add(array[i]);
		}
		try {
			return resolveELOperand(context.getResource(), operand, returnEqualedVariablesOnly, vars, new ElVarSearcher(context.getResource(), this));
		} catch (StringIndexOutOfBoundsException e) {
			log(e);
		} catch (BadLocationException e) {
			log(e);
		}
		return null;
	}

	public ELExpression parseOperand(String operand) {
		return parseOperand(operand, getParserFactory());
	}

	public ELExpression parseOperand(String operand, ELParserFactory factory) {
		if(operand == null) return null;
		String el = (operand.indexOf("#{") < 0 && operand.indexOf("${") < 0) ? "#{" + operand + "}" : operand; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$
		ELParser p = factory.createParser();
		ELModel model = p.parse(el);
		List<ELInstance> is = model.getInstances();
		if(is.isEmpty()) return null;
		return is.get(0).getExpression();
	}

	protected static final String collectionAdditionForCollectionDataModel = ".iterator().next()"; //$NON-NLS-1$
	protected static final String collectionAdditionForMapDataModel = ".entrySet().iterator().next()"; //$NON-NLS-1$

	protected List<String> getVarNameProposals(List <Var> vars, String prefix) {
		List<String> proposals = new ArrayList<String>();
		for (Var var : vars) {
			if(var.getName().startsWith(prefix)) {
				String proposal = var.getName().substring(prefix.length());
				proposals.add(proposal);
			}
		}
		return proposals;
	}

	/*
	 * (non-Javadoc)
	 * @see org.jboss.tools.common.el.core.resolver.ELCompletionEngine#resolveELOperand(org.eclipse.core.resources.IFile, org.jboss.tools.common.el.core.model.ELExpression, boolean, java.util.List, org.jboss.tools.common.el.core.resolver.ElVarSearcher)
	 */
	public ELResolutionImpl resolveELOperand(IFile file,
			ELExpression operand, boolean returnEqualedVariablesOnly,
			List<Var> vars, ElVarSearcher varSearcher)
			throws BadLocationException, StringIndexOutOfBoundsException {
		if(operand == null) {
			//TODO
			return null;
		}
		String oldEl = operand.getText();
		Var var = varSearcher.findVarForEl(oldEl, vars, true);
		String suffix = ""; //$NON-NLS-1$
		String newEl = oldEl;
		TypeInfoCollector.MemberInfo member = null;
		boolean isArray = false;
		ELResolution varELResolution = null;
		if(var!=null) {
			varELResolution = resolveEL(file, var.getElToken(), true);
			if(varELResolution!=null && varELResolution.isResolved()) {
				ELSegment segment = varELResolution.getLastSegment();
				if(segment instanceof JavaMemberELSegment) {
					member = ((JavaMemberELSegment)segment).getMemberInfo();
				}
			}
			if(member!=null) {
				if(!member.getType().isArray()) {
					IType type = member.getMemberType();
					if(type!=null) {
						try {
							if(TypeInfoCollector.isInstanceofType(type, "java.util.Map")) { //$NON-NLS-1$
								suffix = collectionAdditionForMapDataModel;
							} else if(TypeInfoCollector.isInstanceofType(type, "java.util.Collection")) { //$NON-NLS-1$
								suffix = collectionAdditionForCollectionDataModel;
							}
						} catch (JavaModelException e) {
							log(e);
						}
					}
				} else {
					isArray = true;
				}
			}
			if(var.getElToken() != null) {
				newEl = var.getElToken().getText() + suffix + oldEl.substring(var.getName().length());
			}
		}
		boolean prefixWasChanged = !oldEl.equals(newEl);
		if(prefixWasChanged && isArray) {
			member.setDataModel(true);
		}
		ELExpression newOperand = (prefixWasChanged) 
				? ((suffix.length() > 0 && getParserFactory() == ELParserUtil.getDefaultFactory()) 
						? parseOperand(newEl, ELParserUtil.getCollectionFactory()) 
						: parseOperand(newEl)) 
				: operand;

		ELResolutionImpl resolution = resolveELOperand(file, newOperand, returnEqualedVariablesOnly, prefixWasChanged);
		if(resolution==null) {
			return null;
		}

		if(prefixWasChanged) {
			resolution.setSourceOperand(operand);

			// Replace segment which came from var resolution to original first segment.
			LexicalToken firstOriginalToken = operand.getFirstToken();
			LexicalToken nextOriginalToken = firstOriginalToken;
			List<ELSegment> newSegments = resolution.getSegments();
			List<ELSegment> resultSegments = new ArrayList<ELSegment>();
			int startSuffix = var.getElToken().getText().length();
			int endSuffix = startSuffix + suffix.length();
			ELSegment firstSegment = null;
			boolean sufixIsNotResolved = false;
			for (ELSegment segment : newSegments) {
				int startPosition = segment.getToken().getStart();
				if(startPosition>=endSuffix) {
					resultSegments.add(segment);
					nextOriginalToken = nextOriginalToken.findTokenForward(JavaNameTokenDescription.JAVA_NAME);
					((ELSegmentImpl)segment).setToken(nextOriginalToken);
				} else {
					if(!sufixIsNotResolved) {
						sufixIsNotResolved = !segment.isResolved();
					}
					firstSegment = segment;
					((ELSegmentImpl)firstSegment).setToken(firstOriginalToken);
					((ELSegmentImpl)firstSegment).setResolved(!sufixIsNotResolved);
//					if(firstSegment instanceof JavaMemberELSegmentImpl) {
//						JavaMemberELSegmentImpl javaSegment = (JavaMemberELSegmentImpl) firstSegment;
//						MemberInfo m = javaSegment.getMemberInfo();
//						if(m!=null) {
//							TypeInfoCollector.Type t = m.getType();
//							if(t!=null) {
//								javaSegment.setElement(t.getSource());
//							}
//						}
//					}
				}
			}

			if(firstSegment!=null && firstSegment.isResolved()) {
				resultSegments.add(0, firstSegment);
				resolution.setSegments(resultSegments);
				var.resolveValue("#{" + var.getElToken().getText() + suffix + "}"); //$NON-NLS-1$ //$NON-NLS-2$

				ELResolutionImpl oldElResolution = resolveELOperand(file, operand, returnEqualedVariablesOnly, false);
				if(oldElResolution!=null) {
					resolution.getProposals().addAll(oldElResolution.getProposals());
				}
			} else {
				resolution = resolveELOperand(file, operand, returnEqualedVariablesOnly, false);
			}
		}

		if(resolution==null) {
			return null;
		}

		// JBIDE-512, JBIDE-2541 related changes ===>>>
		if(!returnEqualedVariablesOnly && vars!=null) {
			
			for (Var v : vars) {
				String prefix = operand.toString();
				if(v.getName().startsWith(prefix)) {
					ELResolution r = resolveEL(file, v.getElToken(), true, vars, varSearcher);
					ELSegment lastSegment = r.getLastSegment();
					MemberInfo memberInfo = null;
					if(lastSegment instanceof JavaMemberELSegment) {
						memberInfo = ((JavaMemberELSegment)lastSegment).getMemberInfo();
					}

					String sourceTypeName = memberInfo == null ? null : memberInfo.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) {
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					}
					String typeName = memberInfo == null ? null : memberInfo.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) { 
						typeName = Signature.getSimpleName(typeName);
					}

					IJavaElement element = memberInfo == null ? null : memberInfo.getJavaElement();
					String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

					String varNameProposal = v.getName().substring(prefix.length());
					TextProposal proposal = new TextProposal();
					proposal.setLabel(v.getName());
					proposal.setReplacementString(varNameProposal);
					proposal.setLabel(v.getName());
					proposal.setImage(getELProposalImage());
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					proposal.setContextInfo(attachedJavaDoc);
					resolution.getProposals().add(proposal);
				}
			}
		}
		// <<<=== JBIDE-512, JBIDE-2541 related changes

		return resolution;
	}

	/**
	 * Returns ELResolution for EL.
	 * @param seamProject
	 * @param file
	 * @param operand EL without #{}
	 * Returns ELResolution for EL.
	 * @throws BadLocationException
	 * @throws StringIndexOutOfBoundsException
	 */
	public ELResolution resolveEL(IFile file, ELExpression operand, boolean varIsUsed) throws BadLocationException, StringIndexOutOfBoundsException {
		if(!(operand instanceof ELInvocationExpression)) return null;
		return resolveELOperand(file, operand, true, varIsUsed);
	}

	/**
	 * Returns ELResolution for EL.
	 * @param seamProject
	 * @param file
	 * @param operand EL without #{}
	 * @return ELResolution for EL.
	 * @throws BadLocationException
	 * @throws StringIndexOutOfBoundsException
	 */
	public ELResolution resolveEL(IFile file, ELExpression operand, boolean returnEqualedVariablesOnly, List<Var> vars, ElVarSearcher varSearcher) throws BadLocationException, StringIndexOutOfBoundsException {
		if(!(operand instanceof ELInvocationExpression)) return null;
		return resolveELOperand(file, operand, returnEqualedVariablesOnly, vars, varSearcher);
	}

	public ELResolutionImpl resolveELOperand(IFile file, ELExpression operand,  
			boolean returnEqualedVariablesOnly, boolean varIsUsed) throws BadLocationException, StringIndexOutOfBoundsException {
		if(!(operand instanceof ELInvocationExpression) || file == null) {
			return null;
		}

		ELResolutionImpl resolution = new ELResolutionImpl(operand);

		ELInvocationExpression expr = (ELInvocationExpression)operand;
		boolean isIncomplete = expr.getType() == ELObjectType.EL_PROPERTY_INVOCATION 
			&& ((ELPropertyInvocation)expr).getName() == null;
		boolean isArgument = expr.getType() == ELObjectType.EL_ARGUMENT_INVOCATION;

		ELInvocationExpression left = expr;

		List<V> resolvedVariables = new ArrayList<V>();

		if (expr.getLeft() != null && isArgument) {
			left = expr.getLeft();
			resolvedVariables = resolveVariables(file, left, false, 
					true); 	// is Final and equal names are because of 
							// we have no more to resolve the parts of expression, 
							// but we have to resolve arguments of probably a message component
		} else if (expr.getLeft() == null && isIncomplete) {
			resolvedVariables = resolveVariables(file, expr, true, 
					returnEqualedVariablesOnly);
		} else {
			while(left != null) {
				List<V>resolvedVars = new ArrayList<V>();
				resolvedVars = resolveVariables(file, 
						left, left == expr, 
						returnEqualedVariablesOnly);
				if (resolvedVars != null && !resolvedVars.isEmpty()) {
					resolvedVariables = resolvedVars;
					resolution.setLastResolvedToken(left);
					break;
				}
				left = (ELInvocationExpression)left.getLeft();
			} 
		}

		if (resolution.getLastResolvedToken() == null && 
				!returnEqualedVariablesOnly && 
				expr != null && 
				isIncomplete) {
			// no vars are resolved 
			// the tokens are the part of var name ended with a separator (.)
			resolvedVariables = resolveVariables(file, expr, true, returnEqualedVariablesOnly);			

			Set<TextProposal> proposals = new TreeSet<TextProposal>(TextProposal.KB_PROPOSAL_ORDER);
			JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl();
			segment.setToken(expr.getFirstToken());
			segment.setResolved(false);
			resolution.addSegment(segment);
			for (V var : resolvedVariables) {
				String varName = var.getName();
				if(varName.startsWith(operand.getText())) {
					// JBIDE-512, JBIDE-2541 related changes ===>>>

					MemberInfo member = getMemberInfoByVariable(var, true);

					String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					String typeName = member == null ? null : member.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) 
						typeName = Signature.getSimpleName(typeName);

					IJavaElement element = member == null ? null : member.getJavaElement();
					String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

					TextProposal proposal = new TextProposal();
					proposal.setLabel(varName);
					proposal.setReplacementString(varName.substring(operand.getLength()));
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					proposal.setContextInfo(attachedJavaDoc);

					proposals.add(proposal);
					// <<<=== JBIDE-512, JBIDE-2541 related changes
				}
			}
			resolution.setProposals(proposals);
			return resolution;
		}

		// Here we have a list of vars for some part of expression
		// OK. we'll proceed with members of these vars
		if (resolution.getLastResolvedToken() == operand) {
			// First segment is the last one
			Set<TextProposal> proposals = new TreeSet<TextProposal>(TextProposal.KB_PROPOSAL_ORDER);
			// In some cases there may be a few references to the same variable name.
			// For example @Factory and @DataModel. We should use @DataModel instead of @Factory
			// method which returns null.
			// See https://jira.jboss.org/jira/browse/JBIDE-3694

			// JBIDE-512, JBIDE-2541 related changes ===>>>
			TypeInfoCollector.MemberInfo bijectedAttribute = null;

			JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl();
			segment.setToken(operand.getFirstToken());
			segment.setResolved(true);
			resolution.addSegment(segment);

			for (V var : resolvedVariables) {
				if(isSingularAttribute(var)) {
					bijectedAttribute = getMemberInfoByVariable(var, true);
				}
				MemberInfo member = getMemberInfoByVariable(var, true);
				String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
				if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
					sourceTypeName = Signature.getSimpleName(sourceTypeName);
				String typeName = member == null ? null : member.getType().getName();
				if (typeName != null && typeName.indexOf('.') != -1) 
					typeName = Signature.getSimpleName(typeName);
				IJavaElement element = member == null ? null : member.getJavaElement();
				String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;
				
				String varName = var.getName();
				if(operand.getLength()<=varName.length()) {
					TextProposal proposal = new TextProposal();
					proposal.setReplacementString(varName.substring(operand.getLength()));
					proposal.setLabel(varName);
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					proposal.setContextInfo(attachedJavaDoc);
					proposals.add(proposal);
				} else if(returnEqualedVariablesOnly) {
					TextProposal proposal = new TextProposal();
					proposal.setReplacementString(varName);
					proposal.setLabel(varName);
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					proposal.setContextInfo(attachedJavaDoc);
					proposals.add(proposal);
				}
				segment.setMemberInfo(bijectedAttribute!=null?bijectedAttribute:member);
				segment.getVariables().add(var);
			}
			// <<<=== JBIDE-512, JBIDE-2541 related changes

			resolution.setLastResolvedToken(expr);
			resolution.setProposals(proposals);
			return resolution;
		}

		// First segment is found - proceed with next tokens 
		List<TypeInfoCollector.MemberInfo> members = new ArrayList<TypeInfoCollector.MemberInfo>();
		JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl();
		segment.setToken(expr.getFirstToken());
		for (V var : resolvedVariables) {
			TypeInfoCollector.MemberInfo member = getMemberInfoByVariable(var, returnEqualedVariablesOnly);
			if (member != null && !members.contains(member)) { 
				members.add(member);
				segment.setMemberInfo(member);
				segment.getVariables().add(var);
				segment.setResolved(true);
			}
		}
		resolution.addSegment(segment);
		//process segments one by one
		if(left != null) {
			while(left != expr) {
				left = (ELInvocationExpression)left.getParent();
				if (left != expr) { // inside expression
					segment = new JavaMemberELSegmentImpl();
					segment.setResolved(true);
					if(left instanceof ELArgumentInvocation) {
						String s = "#{" + left.getLeft().toString() + collectionAdditionForCollectionDataModel + "}"; //$NON-NLS-1$ //$NON-NLS-2$
						ELParser p = getParserFactory().createParser();
						ELInvocationExpression expr1 = (ELInvocationExpression)p.parse(s).getInstances().get(0).getExpression();
						members = resolveSegment(expr1.getLeft(), members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
						members = resolveSegment(expr1, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
						if(resolution.getLastResolvedToken() == expr1) {
							resolution.setLastResolvedToken(left);
						}
					} else {
						members = resolveSegment(left, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
					}
					if(!members.isEmpty()) {
						segment.setMemberInfo(members.get(0));	// TODO: This is a buggy way to select a member to setup in a segment
					}
					resolution.addSegment(segment);
				} else { // Last segment
					resolveLastSegment((ELInvocationExpression)operand, members, resolution, returnEqualedVariablesOnly, varIsUsed);
					break;
				}
			}
		}

		if(resolution.getProposals().isEmpty() && !resolution.getSegments().isEmpty()) {
//			&& status.getUnpairedGettersOrSetters()!=null) {
			ELSegment lastSegment = resolution.getSegments().get(resolution.getSegments().size()-1);
			if(lastSegment instanceof JavaMemberELSegmentImpl) {
				((JavaMemberELSegmentImpl)lastSegment).clearUnpairedGettersOrSetters();
			}
		}
		return resolution;
	}

	abstract public List<V> resolveVariables(IFile file, ELInvocationExpression expr, boolean isFinal, boolean onlyEqualNames);

	abstract protected TypeInfoCollector.MemberInfo getMemberInfoByVariable(V var, boolean onlyEqualNames);

	abstract protected boolean isStaticMethodsCollectingEnabled();
	
	protected boolean isSingularAttribute(V var) {
		return false;
	}

	protected List<TypeInfoCollector.MemberInfo> resolveSegment(ELInvocationExpression expr, 
			List<TypeInfoCollector.MemberInfo> members,
			ELResolutionImpl resolution,
			boolean returnEqualedVariablesOnly, boolean varIsUsed, JavaMemberELSegmentImpl segment) {
		LexicalToken lt = (expr instanceof ELPropertyInvocation) 
			? ((ELPropertyInvocation)expr).getName()
					: (expr instanceof ELMethodInvocation)
					? ((ELMethodInvocation)expr).getName()
							: null;
		String name = lt != null ? lt.getText() : ""; // token.getText(); //$NON-NLS-1$
		segment.setToken(lt);
		if (expr.getType() == ELObjectType.EL_PROPERTY_INVOCATION) {
			// Find properties for the token
			List<TypeInfoCollector.MemberInfo> newMembers = new ArrayList<TypeInfoCollector.MemberInfo>();
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if (mbr.getMemberType() == null) continue;
				TypeInfoCollector infos = mbr.getTypeCollector(varIsUsed, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}
				List<TypeInfoCollector.MemberInfo> properties = infos.getProperties();
				for (TypeInfoCollector.MemberInfo property : properties) {
					StringBuffer propertyName = new StringBuffer(property.getName());
					if (property instanceof TypeInfoCollector.MethodInfo) { // Setter or getter
						propertyName.delete(0, (propertyName.charAt(0) == 'i' ? 2 : 3));
						propertyName.setCharAt(0, Character.toLowerCase(propertyName.charAt(0)));
					}
					if (name.equals(propertyName.toString())) {
						newMembers.add(property);
					}
				}
			}
			members = newMembers;
			if (members != null && !members.isEmpty())
				resolution.setLastResolvedToken(expr);
		}
		if (expr.getType() == ELObjectType.EL_METHOD_INVOCATION) {
			// Find methods for the token
			if (name.indexOf('(') != -1) {
				name = name.substring(0, name.indexOf('('));
			}
			List<TypeInfoCollector.MemberInfo> newMembers = new ArrayList<TypeInfoCollector.MemberInfo>();
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if (mbr.getMemberType() == null) continue;
				TypeInfoCollector infos = mbr.getTypeCollector(false, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}
				List<TypeInfoCollector.MemberInfo> methods = infos.getMethods();
				for (TypeInfoCollector.MemberInfo method : methods) {
					if (name.equals(method.getName())) {
						newMembers.add(method);
					}
				}
			}
			members = newMembers;
			if (members != null && !members.isEmpty())
				resolution.setLastResolvedToken(expr);
		}
		return members;
	}

	protected void resolveLastSegment(ELInvocationExpression expr, 
			List<TypeInfoCollector.MemberInfo> members,
			ELResolutionImpl resolution,
			boolean returnEqualedVariablesOnly, boolean varIsUsed) {
		Set<TextProposal> kbProposals = new TreeSet<TextProposal>(TextProposal.KB_PROPOSAL_ORDER);

		JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl();
		if(expr instanceof ELPropertyInvocation) {
			segment.setToken(((ELPropertyInvocation)expr).getName());
		}
//		segment.setToken(expr.getLastToken());
		if(segment.getToken()!=null) {
			resolution.addSegment(segment);
		}
		resolution.setProposals(kbProposals);

		if (expr.getType() == ELObjectType.EL_PROPERTY_INVOCATION && ((ELPropertyInvocation)expr).getName() == null) {
			// return all the methods + properties
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if(isSingularMember(mbr)) {
					processSingularMember(mbr, kbProposals);
					continue;
				}
				if (mbr.getMemberType() == null) {
					continue;
				}
				TypeInfoCollector infos = mbr.getTypeCollector(varIsUsed, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}

				// JBIDE-512, JBIDE-2541 related changes ===>>>
				/*
				Set<String> methodPresentations = 
						infos.getMethodPresentationStrings();
				if (methodPresentations != null) {
					for (String presentation : methodPresentations) {
						TextProposal proposal = new TextProposal();
						proposal.setReplacementString(presentation);
						proposal.setImage(getELProposalImage());
						
						kbProposals.add(proposal);
					}
				}
				*/

				Set<MemberPresentation> methodPresentations = 
					infos.getMethodPresentations();

				if (methodPresentations != null) {
					for (MemberPresentation presentation : methodPresentations) {
						String presentationString = presentation.getPresentation();
						MemberInfo member = presentation.getMember();
						String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
						if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
							sourceTypeName = Signature.getSimpleName(sourceTypeName);
						String typeName = member == null ? null : member.getType().getName();
						if (typeName != null && typeName.indexOf('.') != -1) 
							typeName = Signature.getSimpleName(typeName);
						IJavaElement element = member == null ? null : member.getJavaElement();
						String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

						TextProposal proposal = new TextProposal();
						proposal.setReplacementString(presentationString);
						proposal.setLabel(presentationString);
						proposal.setImage(getELProposalImage());
						proposal.setType(typeName);
						proposal.setSourceType(sourceTypeName);
						proposal.setContextInfo(attachedJavaDoc);

						kbProposals.add(proposal);
					}
				}

				/*
				Set<String> propertyPresentations = 
					infos.getPropertyPresentationStrings(status.getUnpairedGettersOrSetters());
				if (propertyPresentations != null) {
					for (String presentation : propertyPresentations) {
						TextProposal proposal = new TextProposal();
						proposal.setReplacementString(presentation);
						proposal.setImage(getELProposalImage());
						
						kbProposals.add(proposal);
					}
				}
				*/

				Set<MemberPresentation> propertyPresentations = 
					infos.getPropertyPresentations(segment.getUnpairedGettersOrSetters());

				if (propertyPresentations != null) {
					for (MemberPresentation presentation : propertyPresentations) {
						String presentationString = presentation.getPresentation();
						MemberInfo member = presentation.getMember();
						String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
						if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
							sourceTypeName = Signature.getSimpleName(sourceTypeName);
						String typeName = member == null ? null : member.getType().getName();
						if (typeName != null && typeName.indexOf('.') != -1) 
							typeName = Signature.getSimpleName(typeName);
						IJavaElement element = member == null ? null : member.getJavaElement();
						String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

						TextProposal proposal = new TextProposal();
						proposal.setReplacementString(presentationString);
						proposal.setLabel(presentationString);
						proposal.setImage(getELProposalImage());
						proposal.setType(typeName);
						proposal.setSourceType(sourceTypeName);
						proposal.setContextInfo(attachedJavaDoc);

						kbProposals.add(proposal);
					}
				}
				// <<<=== JBIDE-512, JBIDE-2541 related changes
			}
		} else
			if(expr.getType() != ELObjectType.EL_ARGUMENT_INVOCATION)
			//actually any case
//			if (token.getType() == ELOperandToken.EL_VARIABLE_NAME_TOKEN ||
//				token.getType() == ELOperandToken.EL_PROPERTY_NAME_TOKEN ||
//				token.getType() == ELOperandToken.EL_METHOD_TOKEN) 
			{
			// return filtered methods + properties 
			Set<TypeInfoCollector.MemberPresentation> proposalsToFilter = new TreeSet<TypeInfoCollector.MemberPresentation>(TypeInfoCollector.MEMBER_PRESENTATION_COMPARATOR);
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if(isSingularMember(mbr)) {
					filterSingularMember(mbr, proposalsToFilter);
					continue;
				}
				if (mbr.getMemberType() == null) continue;
				TypeInfoCollector infos = mbr.getTypeCollector(false, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}
				proposalsToFilter.addAll(infos.getMethodPresentations());
				proposalsToFilter.addAll(infos.getPropertyPresentations(segment.getUnpairedGettersOrSetters()));
//				segment.setMemberInfo(mbr);
			}
			for (TypeInfoCollector.MemberPresentation proposal : proposalsToFilter) {
				// We do expect nothing but name for method tokens (No round brackets)
				String filter = expr.getMemberName();
				if(filter == null) filter = ""; //$NON-NLS-1$
				if(returnEqualedVariablesOnly) {
					// This is used for validation.
					if (proposal.getPresentation().equals(filter)) {
						TextProposal kbProposal = new TextProposal();
						kbProposal.setReplacementString(proposal.getPresentation());

						setImage(kbProposal, proposal);

						kbProposals.add(kbProposal);

						segment.setMemberInfo(proposal.getMember());
						if(segment.getUnpairedGettersOrSetters()!=null) {
							TypeInfoCollector.MethodInfo unpirMethod = segment.getUnpairedGettersOrSetters().get(filter);
							segment.clearUnpairedGettersOrSetters();
							if(unpirMethod!=null) {
								segment.getUnpairedGettersOrSetters().put(filter, unpirMethod);
							}
						}
						break;
					}
				} else if (proposal.getPresentation().startsWith(filter)) {
					// JBIDE-512, JBIDE-2541 related changes ===>>>

					// This is used for CA.
					MemberInfo member = proposal.getMember();
					String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					String typeName = member == null ? null : member.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) 
						typeName = Signature.getSimpleName(typeName);
					IJavaElement element = member == null ? null : member.getJavaElement();
					String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

					TextProposal kbProposal = new TextProposal();
					kbProposal.setReplacementString(proposal.getPresentation().substring(filter.length()));
					kbProposal.setLabel(proposal.getPresentation());
					kbProposal.setImage(getELProposalImage());
					kbProposal.setType(typeName);
					kbProposal.setSourceType(sourceTypeName);
					kbProposal.setContextInfo(attachedJavaDoc);

					kbProposals.add(kbProposal);
					// <<<=== JBIDE-512, JBIDE-2541 related changes
				}
			}
		} else if(expr.getType() == ELObjectType.EL_ARGUMENT_INVOCATION) {
			Set<TypeInfoCollector.MemberPresentation> proposalsToFilter = new TreeSet<TypeInfoCollector.MemberPresentation>(TypeInfoCollector.MEMBER_PRESENTATION_COMPARATOR);
			boolean isMessages = false;
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if(isSingularMember(mbr)) {
					isMessages = true;
					filterSingularMember(mbr, proposalsToFilter);
					continue;
				}
				if (mbr.getMemberType() == null) continue;
				try {
					if(TypeInfoCollector.isInstanceofType(mbr.getMemberType(), "java.util.Map")) { //$NON-NLS-1$
						resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
						//if map/collection is parameterized, we might return member info for value type. 
						return;
					}
				} catch (JavaModelException jme) {
					ELCorePlugin.getDefault().logError(jme);
				}
				segment.setMemberInfo(mbr);
			}

			String filter = expr.getMemberName();
			boolean bSurroundWithQuotes = false;
			if(filter == null) {
				filter = ""; //$NON-NLS-1$
				bSurroundWithQuotes = true;
			} else {
				if((filter.startsWith("'") || filter.startsWith("\"")) //$NON-NLS-1$ //$NON-NLS-2$
					&& (filter.endsWith("'") || filter.endsWith("\""))) { //$NON-NLS-1$ //$NON-NLS-2$
					filter = filter.length() == 1 ? "" : filter.substring(1, filter.length() - 1); //$NON-NLS-1$
				} else {
					//Value is set as expression itself, we cannot compute it
					if(isMessages) {
						resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
					}
					return;
				}
			}

			for (TypeInfoCollector.MemberPresentation proposal : proposalsToFilter) {
				if(returnEqualedVariablesOnly) {
					// This is used for validation.
					if (proposal.getPresentation().equals(filter)) {
						TextProposal kbProposal = new TextProposal();
						kbProposal.setReplacementString(proposal.getPresentation());

						setImage(kbProposal, proposal);

						kbProposals.add(kbProposal);

						segment.setMemberInfo(proposal.getMember());
						if(segment.getUnpairedGettersOrSetters()!=null) {
							TypeInfoCollector.MethodInfo unpirMethod = segment.getUnpairedGettersOrSetters().get(filter);
							segment.clearUnpairedGettersOrSetters();
							if(unpirMethod!=null) {
								segment.getUnpairedGettersOrSetters().put(filter, unpirMethod);
							}
						}
						break;
					}
				} else if (proposal.getPresentation().startsWith(filter)) {
					// This is used for CA.
					// JBIDE-512, JBIDE-2541 related changes ===>>>

					MemberInfo member = proposal.getMember();
					String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					String typeName = member == null ? null : member.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) 
						typeName = Signature.getSimpleName(typeName);
					IJavaElement element = member == null ? null : member.getJavaElement();
					String attachedJavaDoc = (element instanceof IMember)?(new ProposalInfo((IMember)element)).getInfo(null):null;

					TextProposal kbProposal = new TextProposal();

					String replacementString = proposal.getPresentation().substring(filter.length());
					if (bSurroundWithQuotes) {
						replacementString = "'" + replacementString + "']"; //$NON-NLS-1$ //$NON-NLS-2$
					}

					kbProposal.setReplacementString(replacementString);
					kbProposal.setImage(getELProposalImage());
					kbProposal.setType(typeName);
					kbProposal.setSourceType(sourceTypeName);
					kbProposal.setContextInfo(attachedJavaDoc);

					kbProposals.add(kbProposal);
					// <<<=== JBIDE-512, JBIDE-2541 related changes
				}
			}
		}
		segment.setResolved(!resolution.getProposals().isEmpty() || resolution.isMapOrCollectionOrBundleAmoungTheTokens());
	}

	protected boolean isSingularMember(TypeInfoCollector.MemberInfo mbr) {
		return false;
	}

	protected void processSingularMember(TypeInfoCollector.MemberInfo mbr, Set<TextProposal> kbProposals) {
	}

	protected void filterSingularMember(TypeInfoCollector.MemberInfo mbr, Set<TypeInfoCollector.MemberPresentation> proposalsToFilter) {
	}

	protected void setImage(TextProposal kbProposal, TypeInfoCollector.MemberPresentation proposal) {
		kbProposal.setImage(getELProposalImage());
	}

	protected void setImage(TextProposal kbProposal, V var) {
		kbProposal.setImage(getELProposalImage());
	}

	/**
	 * 
	 * @param document
	 * @param offset
	 * @param start  start of relevant region in document
	 * @param end    end of relevant region in document
	 * @return
	 */
	public static ELInvocationExpression findExpressionAtOffset(IDocument document, int offset, int start, int end) {
		return findExpressionAtOffset(document.get(), offset, start, end);
	}

	public static ELInvocationExpression findExpressionAtOffset(String content, int offset, int start, int end) {
		//TODO this naive calculations should be removed; 
		//	   this method should be called with reasonable start and end. 
		if(start <= 0) start = guessStart(content, offset);
		if(end >= content.length()) end = guessEnd(content, offset);
		
		ELParser parser = defaultFactory.createParser();
		ELModel model = parser.parse(content, start, end - start);
		
		return ELUtil.findExpression(model, offset);
	}

	static int guessStart(String content, int offset) {
		if(offset > content.length()) offset = content.length();
		if(offset < 2) return 0;
		int s = offset - 2;
		
		while(s >= 0) {
			if(content.charAt(s + 1) == '{') {
				char ch = content.charAt(s);
				if(ch == '#' || ch == '$') return s;
			}
			s--;
		}
		return 0;
	}

	static int guessEnd(String content, int offset) {
		if(offset >= content.length()) return content.length();
		while(offset < content.length()) {
			if(content.charAt(offset) == '}') return offset;
			offset++;
		}
		return content.length();
	}

	/**
	 * Removes duplicates of completion strings
	 *
	 * @param suggestions a list of suggestions ({@link String}).
	 * @return a list of unique completion suggestions.
	 */
	public static List<TextProposal> makeProposalsUnique(List<TextProposal> suggestions) {
		HashSet<String> present = new HashSet<String>();
		ArrayList<TextProposal> unique= new ArrayList<TextProposal>();

		if (suggestions == null)
			return unique;

		for (TextProposal item : suggestions) {
			if (!present.contains(item.getReplacementString())) {
				present.add(item.getReplacementString());
				unique.add(item);
			}
		}

		present.clear();
		return unique;
	}
}