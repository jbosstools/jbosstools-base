/******************************************************************************* 
 * Copyright (c) 2007-2011 Red Hat, Inc. 
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
import org.eclipse.core.resources.IResource;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IType;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.Signature;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.jboss.tools.common.el.core.ELCorePlugin;
import org.jboss.tools.common.el.core.ELReference;
import org.jboss.tools.common.el.core.model.ELArgument;
import org.jboss.tools.common.el.core.model.ELArgumentInvocation;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.model.ELInstance;
import org.jboss.tools.common.el.core.model.ELInvocationExpression;
import org.jboss.tools.common.el.core.model.ELMethodInvocation;
import org.jboss.tools.common.el.core.model.ELModel;
import org.jboss.tools.common.el.core.model.ELObject;
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
import org.jboss.tools.common.el.core.resolver.IRelevanceCheck;
import org.jboss.tools.common.el.core.resolver.IVariable;
import org.jboss.tools.common.el.core.resolver.JavaMemberELSegment;
import org.jboss.tools.common.el.core.resolver.JavaMemberELSegmentImpl;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberInfo;
import org.jboss.tools.common.el.core.resolver.TypeInfoCollector.MemberPresentation;
import org.jboss.tools.common.el.core.resolver.Var;
import org.jboss.tools.common.el.internal.core.parser.token.JavaNameTokenDescription;
import org.jboss.tools.common.el.internal.core.parser.token.WhiteSpaceTokenDescription;
import org.jboss.tools.common.text.TextProposal;
import org.jboss.tools.common.util.BeanUtil;

public abstract class AbstractELCompletionEngine<V extends IVariable> implements ELResolver, ELCompletionEngine {
	
	public static final IRelevanceCheck IRRELEVANT = new IRelevanceCheck() {
		public boolean isRelevant(String content) {
			return false;
		}
	};
		
	public AbstractELCompletionEngine() {}

	protected abstract ImageDescriptor getELProposalImageForMember(MemberInfo memberInfo);

	protected abstract void log(Exception e);

	private static ELParserFactory defaultFactory = ELParserUtil.getJbossFactory();

	protected ImageDescriptor getELProposalImage(MemberPresentation memberPresentation) {
		return getELProposalImageForMember(memberPresentation!=null?memberPresentation.getMember():null);
	}

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
			List<ELInstance> is = ref.getELModel().getInstances();
			for (ELInstance i: is) {
				ELExpression exp = i.getExpression();
				if(exp == null || exp.getFirstToken() == null) {
					continue;
				}
				LexicalToken b = exp.getFirstToken();
				while(b.getPreviousToken() != null && b.getPreviousToken().getType() == WhiteSpaceTokenDescription.WHITESPACE) {
					b = b.getPreviousToken();
				}
				LexicalToken e = exp.getLastToken();
				if(e == null) e = b;
				while(e.getNextToken() != null && e.getNextToken().getType() == WhiteSpaceTokenDescription.WHITESPACE) {
					e = e.getNextToken();
				}
				if(exp != null && ref.getStartPosition() + b.getStart() <= offset && ref.getStartPosition() + e.getStart() + e.getLength() >= offset) {
					return getProposals(context, "#{", offset); //$NON-NLS-1$
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
			resolution = resolveELOperand(context.getResource(), context, parseOperand(el), false, vars, new ElVarSearcher(context.getResource(), this), offset);
			if(resolution!=null) {
				completions.addAll(resolution.getProposals());
			}
		} catch (StringIndexOutOfBoundsException e) {
			log(e);
		} catch (BadLocationException e) {
			log(e);
		}
		
		return completions;
	}

	public IRelevanceCheck createRelevanceCheck(IJavaElement element) {
		return new DefaultJavaRelevanceCheck(element);
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
			resolution = resolveELOperand(context.getResource(), context, operand, true, vars, new ElVarSearcher(context.getResource(), this), offset);
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
			return resolveELOperand(context.getResource(), context, operand, returnEqualedVariablesOnly, vars, new ElVarSearcher(context.getResource(), this), offset);
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
		if(operand == null || factory == null) return null;
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
	public ELResolutionImpl resolveELOperand(IFile file, ELContext context,
			ELExpression operand, boolean returnEqualedVariablesOnly,
			List<Var> vars, ElVarSearcher varSearcher, int offset)
			throws BadLocationException, StringIndexOutOfBoundsException {
		if(operand == null) {
			//TODO
			return null;
		}
		String oldEl = operand.getText();
		Var var = varSearcher.findVarForEl(oldEl, context, vars, true);
		String suffix = ""; //$NON-NLS-1$
		String newEl = oldEl;
		TypeInfoCollector.MemberInfo member = null;
		boolean isArray = false;
		ELResolution varELResolution = null;
		if(var!=null) {
			varELResolution = resolveEL(file, context, var.getElToken(), true, offset);
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
							} else if(TypeInfoCollector.isInstanceofType(type, "java.lang.Iterable")) { //$NON-NLS-1$
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

		ELResolutionImpl resolution = resolveELOperand(file, context, newOperand, returnEqualedVariablesOnly, prefixWasChanged, offset);
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
					((ELSegmentImpl)firstSegment).setVars(varSearcher.findVarsForEl(oldEl, context, vars, true));
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

				// Save all used variables from "value" EL to the list of used variables for EL which uses this "var" attribute.
				if(firstSegment.getVariables()!=null) {
					for (ELSegment segment : varELResolution.getSegments()) {
						if(segment.getVariables()!=null) {
							firstSegment.getVariables().addAll(segment.getVariables());
						}
					}
				}

				ELResolutionImpl oldElResolution = resolveELOperand(file, context, operand, returnEqualedVariablesOnly, false, offset);
				if(oldElResolution!=null) {
					resolution.getProposals().addAll(oldElResolution.getProposals());
				}
			} else {
				resolution = resolveELOperand(file, context, operand, returnEqualedVariablesOnly, false, offset);
				if(var != null && !resolution.getSegments().isEmpty()) {
					((ELSegmentImpl)resolution.getSegments().get(0)).setVars(varSearcher.findVarsForEl(oldEl, context, vars, true));
				}
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
					ELResolution r = resolveEL(file, context, v.getElToken(), true, vars, varSearcher, offset);
					if(r==null) {
						continue;
					}
					ELSegment lastSegment = r.getLastSegment();
					JavaMemberELSegment jmSegment = null;
					
					if(lastSegment instanceof JavaMemberELSegment) {
						jmSegment = ((JavaMemberELSegment)lastSegment);
					}
  					MemberInfo memberInfo = jmSegment == null ? null : jmSegment.getMemberInfo();

					String sourceTypeName = memberInfo == null ? null : memberInfo.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) {
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					}
					String typeName = memberInfo == null ? null : memberInfo.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) { 
						typeName = Signature.getSimpleName(typeName);
					}

					String varNameProposal = v.getName().substring(prefix.length());
					ELTextProposal proposal = new ELTextProposal();
					proposal.setLabel(v.getName());
					proposal.setReplacementString(varNameProposal);
					proposal.setLabel(v.getName());
					proposal.setImageDescriptor(getELProposalImageForMember(memberInfo));
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					if (jmSegment != null) {
						IJavaElement[] javaElements = jmSegment.getAllJavaElements();
						for (int jeIndex = 0; javaElements != null && jeIndex < javaElements.length; jeIndex++) {
							proposal.addJavaElement(javaElements[jeIndex]);
						}
					}
					
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
	public ELResolution resolveEL(IFile file, ELContext context, ELExpression operand, boolean varIsUsed, int offset) throws BadLocationException, StringIndexOutOfBoundsException {
		if(!(operand instanceof ELInvocationExpression)) return null;
		return resolveELOperand(file, context, operand, true, varIsUsed, offset);
	}

	/**
	 * Returns ELResolution for EL.
	 * @param file
	 * @param operand EL without #{}
	 * @param offset TODO
	 * @param seamProject
	 * @return ELResolution for EL.
	 * @throws BadLocationException
	 * @throws StringIndexOutOfBoundsException
	 */
	public ELResolution resolveEL(IFile file, ELContext context, ELExpression operand, boolean returnEqualedVariablesOnly, List<Var> vars, ElVarSearcher varSearcher, int offset) throws BadLocationException, StringIndexOutOfBoundsException {
		if(!(operand instanceof ELInvocationExpression)) return null;
		return resolveELOperand(file, context, operand, returnEqualedVariablesOnly, vars, varSearcher, offset);
	}

	public ELResolutionImpl resolveELOperand(IFile file, ELContext context, ELExpression operand,  
			boolean returnEqualedVariablesOnly, boolean varIsUsed, int offset) throws BadLocationException, StringIndexOutOfBoundsException {
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
			//argument may be applied not to a variable but to a complex expression.
			left = expr.getLeft();
//			resolvedVariables = resolveVariables(file, left, false, 
//					true, offset); 	// is Final and equal names are because of 
//							// we have no more to resolve the parts of expression, 
//							// but we have to resolve arguments of probably a message component
		} //else 
		if (expr.getLeft() == null && isIncomplete) {
			resolvedVariables = resolveVariables(file, context, expr, true, 
					returnEqualedVariablesOnly, offset);
		} else {
			while(left != null) {
				List<V>resolvedVars = new ArrayList<V>();
				resolvedVars = resolveVariables(file, context,
						left, left == expr, 
						returnEqualedVariablesOnly, offset);
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
			resolvedVariables = resolveVariables(file, context, expr, true, returnEqualedVariablesOnly, offset);			

			Set<TextProposal> proposals = new TreeSet<TextProposal>(TextProposal.KB_PROPOSAL_ORDER);
			JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl(expr.getFirstToken());
			segment.setResolved(false);
			resolution.addSegment(segment);
			for (V var : resolvedVariables) {
				String varName = var.getName();
				if(varName.startsWith(operand.getText())) {
					// JBIDE-512, JBIDE-2541 related changes ===>>>

					MemberInfo member = getMemberInfoByVariable(var, context, true, offset);

					String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					String typeName = member == null ? null : member.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) 
						typeName = Signature.getSimpleName(typeName);

					IJavaElement element = member == null ? null : member.getJavaElement();

					ELTextProposal proposal = new ELTextProposal();
					proposal.setLabel(varName);
					proposal.setReplacementString(varName.substring(operand.getLength()));
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					if (element != null) {
						proposal.addJavaElement(element);
					}
					
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

			LexicalToken t = operand.getFirstToken();
			if(t != null && t != operand.getLastToken() && operand.getLastToken() != null) {
				t = t.getCombinedToken(operand.getLastToken());
			}
			JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl(t);
			segment.setResolved(true);
			resolution.addSegment(segment);

			for (V var : resolvedVariables) {
				if(isSingularAttribute(var)) {
					bijectedAttribute = getMemberInfoByVariable(var, context, true, offset);
				}

				MemberInfo member = getMemberInfoByVariable(var, context, true, offset);
				String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
				if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
					sourceTypeName = Signature.getSimpleName(sourceTypeName);
				String typeName = member == null ? null : member.getType().getName();
				if (typeName != null && typeName.indexOf('.') != -1) 
					typeName = Signature.getSimpleName(typeName);
					
				IJavaElement element = member == null ? null : member.getJavaElement();

				String varName = var.getName();
				if(operand.getLength()<=varName.length()) {
					ELTextProposal proposal = new ELTextProposal();
					proposal.setReplacementString(varName.substring(operand.getLength()));
					proposal.setLabel(varName);
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					if (element != null) {
						proposal.addJavaElement(element);
					}

					proposals.add(proposal);
				} else if(returnEqualedVariablesOnly) {
					ELTextProposal proposal = new ELTextProposal();
					proposal.setReplacementString(varName);
					proposal.setLabel(varName);
					setImage(proposal, var);
					proposal.setType(typeName);
					proposal.setSourceType(sourceTypeName);
					if (element != null) {
						proposal.addJavaElement(element);
					}

					proposals.add(proposal);
				}
				if(member!=null || bijectedAttribute!=null) {
					segment.setMemberInfo(bijectedAttribute!=null?bijectedAttribute:member);
				}
				segment.getVariables().add(var);
			}
			// <<<=== JBIDE-512, JBIDE-2541 related changes

			resolution.setLastResolvedToken(expr);
			resolution.setProposals(proposals);
			return resolution;
		}

		// First segment is found - proceed with next tokens 
		List<TypeInfoCollector.MemberInfo> members = new ArrayList<TypeInfoCollector.MemberInfo>();
		JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl(expr.getFirstToken());
		for (V var : resolvedVariables) {
			TypeInfoCollector.MemberInfo member = getMemberInfoByVariable(var, context, returnEqualedVariablesOnly, offset);
			if (member != null && !members.contains(member)) { 
				String name = var.getName();
				if(name.indexOf('.') >= 0) {
					LexicalToken last = expr.getFirstToken();
					StringBuffer sb = new StringBuffer();
					sb.append(last.getText());
					while(!name.equals(sb.toString()) && last != null) {
						last = last.getNextToken();
						if(last != null) {
							sb.append(last.getText());
						}
					}
					if(last != null && name.equals(sb.toString())) {
						segment = new JavaMemberELSegmentImpl(expr.getFirstToken().getCombinedToken(last));
					}
				}
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
					JavaMemberELSegmentImpl lastSegment = segment;
					segment = new JavaMemberELSegmentImpl(left.getLastToken());
					boolean skipSegment = false;
					if(left instanceof ELArgumentInvocation) { 
						List<MemberInfo> ms = new ArrayList<MemberInfo>(members);
						members.clear();
						
						// The segment could have a collection or a map data model (or probably both?)
						MemberInfo lastSegmentMemberInfo = lastSegment == null ? null : lastSegment.getMemberInfo();
						if (lastSegmentMemberInfo != null) {
							if(!lastSegmentMemberInfo.getType().isArray()) {
								IType type = lastSegmentMemberInfo.getMemberType();
								if(type!=null) {
									try {
										if(TypeInfoCollector.isInstanceofType(type, "java.util.Map")) { //$NON-NLS-1$
											skipSegment = true;
											String s = "#{" + left.getLeft().toString() + ".values().iterator().next()}"; //$NON-NLS-1$ //$NON-NLS-2$
											if(getParserFactory()!=null) {
												ELParser p = getParserFactory().createParser();
												ELInvocationExpression expr1 = (ELInvocationExpression)p.parse(s).getInstances().get(0).getExpression();
												members = resolveSegment(expr1.getLeft().getLeft(), ms, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
												members = resolveSegment(expr1.getLeft(), members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
												members = resolveSegment(expr1, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
												if(resolution.getLastResolvedToken() == expr1) {
													resolution.setLastResolvedToken(left);
												}
											}
											segment = lastSegment;
										} else if(TypeInfoCollector.isInstanceofType(type, "java.lang.Iterable")) { //$NON-NLS-1$
											skipSegment = true;
											String s = "#{" + left.getLeft().toString() + collectionAdditionForCollectionDataModel + "}"; //$NON-NLS-1$ //$NON-NLS-2$
											if(getParserFactory()!=null) {
												ELParser p = getParserFactory().createParser();
												ELInvocationExpression expr1 = (ELInvocationExpression)p.parse(s).getInstances().get(0).getExpression();
												members = resolveSegment(expr1.getLeft(), ms, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
												members = resolveSegment(expr1, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
												if(resolution.getLastResolvedToken() == expr1) {
													resolution.setLastResolvedToken(left);
												}
											}
											segment = lastSegment;
										}
									} catch (JavaModelException e) {
										log(e);
									}
								}
							}
						}
/*						
						String s = "#{" + left.getLeft().toString() + collectionAdditionForCollectionDataModel + "}"; //$NON-NLS-1$ //$NON-NLS-2$
						if(getParserFactory()!=null) {
							ELParser p = getParserFactory().createParser();
							ELInvocationExpression expr1 = (ELInvocationExpression)p.parse(s).getInstances().get(0).getExpression();
							members = resolveSegment(expr1.getLeft(), members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
							members = resolveSegment(expr1, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
							if(resolution.getLastResolvedToken() == expr1) {
								resolution.setLastResolvedToken(left);
							}
						}
*/						
						if(members.isEmpty()) {
							members = resolveSegment(left, ms, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
						}
					} else {
						members = resolveSegment(left, members, resolution, returnEqualedVariablesOnly, varIsUsed, segment);
					}
					if(!skipSegment) { // Do not store any members if the segment is 'skipped' because it's already resolved.
						if (!members.isEmpty()) {
							segment.setResolved(true);
							segment.setMemberInfo(members.get(0));	// TODO: This is a buggy way to select a member to setup in a segment
						}

						resolution.addSegment(segment);
					}
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

	abstract public List<V> resolveVariables(IFile file, ELContext context, ELInvocationExpression expr, boolean isFinal, boolean onlyEqualNames, int offset);

	abstract protected TypeInfoCollector.MemberInfo getMemberInfoByVariable(V var, ELContext context, boolean onlyEqualNames, int offset);

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
					: (expr instanceof ELArgumentInvocation && ((ELArgumentInvocation)expr).getArgument().getArgument() != null)
					? ((ELArgumentInvocation)expr).getArgument().getArgument().getFirstToken()
							: null;
		String name = lt != null ? lt.getText() : ""; // token.getText(); //$NON-NLS-1$
		if(expr instanceof ELArgumentInvocation) {
			if(name.startsWith("'")) name = name.substring(1); else name = ""; //$NON-NLS-1$ //$NON-NLS-2$
			if(name.endsWith("'")) name = name.substring(0, name.length() - 1); else name = ""; //$NON-NLS-1$ //$NON-NLS-2$
		}
		segment.setToken(lt);
		if (expr.getType() == ELObjectType.EL_PROPERTY_INVOCATION || expr.getType() == ELObjectType.EL_ARGUMENT_INVOCATION) {
			// Find properties for the token
			List<TypeInfoCollector.MemberInfo> newMembers = new ArrayList<TypeInfoCollector.MemberInfo>();
			for (TypeInfoCollector.MemberInfo mbr : members) {
				if (mbr.getMemberType() == null) continue;
				ICompilationUnit unit = mbr.getMemberType().getCompilationUnit();
				if(unit!=null && unit.exists()) {
					IResource resource;
					try {
						resource = unit.getCorrespondingResource();
						if(resource!=null) {
							segment.setResource(resource);						
						}
					} catch (JavaModelException e) {
						ELCorePlugin.getDefault().logError(e);
					}
				}
				TypeInfoCollector infos = mbr.getTypeCollector(varIsUsed, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}
				List<TypeInfoCollector.MemberInfo> properties = infos.getProperties();
				for (TypeInfoCollector.MemberInfo property : properties) {
					String propertyName = property.getName();
					if (property instanceof TypeInfoCollector.MethodInfo) { // Setter or getter
						propertyName = BeanUtil.getPropertyName(propertyName);
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

		JavaMemberELSegmentImpl segment = new JavaMemberELSegmentImpl(null);
		if(expr instanceof ELPropertyInvocation) {
			segment.setToken(((ELPropertyInvocation)expr).getName());
		}
		if(segment.getToken()!=null) {
			resolution.addSegment(segment);
		} else {
			segment.setToken(expr.getLastToken());
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
					infos.getMethodPresentations(returnEqualedVariablesOnly);

				if (methodPresentations != null) {
					for (MemberPresentation presentation : methodPresentations) {
						String presentationString = presentation.getPresentation();
						String presentationDisplayName = presentation.getPresentationDisplayName();
						MemberInfo member = presentation.getMember();
						String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
						if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
							sourceTypeName = Signature.getSimpleName(sourceTypeName);
						String typeName = member == null ? null : member.getType().getName();
						if (typeName != null && typeName.indexOf('.') != -1) 
							typeName = Signature.getSimpleName(typeName);

						ELTextProposal proposal = new ELTextProposal();
						proposal.setReplacementString(presentationString);
						proposal.setLabel(presentationDisplayName);
						proposal.setImageDescriptor(getELProposalImage(presentation));
						proposal.setType(typeName);
						proposal.setSourceType(sourceTypeName);
						for (MemberInfo mi : presentation.getAllMembers()) {
							IJavaElement element = mi.getJavaElement();
							if (element != null) {
								proposal.addJavaElement(element);
							}
						}

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
					infos.getPropertyPresentations(segment.getUnpairedGettersOrSetters(), returnEqualedVariablesOnly);

				if (propertyPresentations != null) {
					for (MemberPresentation presentation : propertyPresentations) {
						String presentationString = presentation.getPresentation();
						String presentationDisplayName = presentation.getPresentationDisplayName();
						MemberInfo member = presentation.getMember();
						String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
						if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
							sourceTypeName = Signature.getSimpleName(sourceTypeName);
						String typeName = member == null ? null : member.getType().getName();
						if (typeName != null && typeName.indexOf('.') != -1) 
							typeName = Signature.getSimpleName(typeName);

						ELTextProposal proposal = new ELTextProposal();
						proposal.setReplacementString(presentationString);
						proposal.setLabel(presentationDisplayName);
						proposal.setImageDescriptor(getELProposalImage(presentation));
						proposal.setType(typeName);
						proposal.setSourceType(sourceTypeName);
						for (MemberInfo mi : presentation.getAllMembers()) {
							IJavaElement element = mi.getJavaElement();
							if (element != null) {
								proposal.addJavaElement(element);
							}
						}
						
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
				proposalsToFilter.addAll(infos.getMethodPresentations(returnEqualedVariablesOnly));
				proposalsToFilter.addAll(infos.getPropertyPresentations(segment.getUnpairedGettersOrSetters(), returnEqualedVariablesOnly));
//				segment.setMemberInfo(mbr);
			}
			
			for (TypeInfoCollector.MemberPresentation proposal : proposalsToFilter) {
				// We do expect nothing but name for method tokens (No round brackets)
				String filter = expr.getMemberName();
				if(filter == null) filter = ""; //$NON-NLS-1$
				String presentationString = proposal.getPresentation();
				//proposal.getMember().getSourceType().getCompilationUnit().getCorrespondingResource()
				MemberInfo info = proposal.getMember();
				if(info!=null) {
					IType type = info.getSourceType();
					if(type!=null) {
						ICompilationUnit unit = type.getCompilationUnit();
						if(unit!=null && unit.exists()) {
							try {
								IResource resource = unit.getCorrespondingResource();
								if(resource!=null) {
									segment.setResource(resource);
								}
							} catch (JavaModelException e) {
								ELCorePlugin.getDefault().logError(e);
							}
						}
					}
				}
				if(returnEqualedVariablesOnly) {
					// This is used for validation.
					if (presentationString.equals(filter)) {
						TextProposal kbProposal = new TextProposal();
						kbProposal.setReplacementString(proposal.getPresentation());

						setImage(kbProposal, proposal);

						kbProposals.add(kbProposal);

						segment.setMemberInfo(proposal.getMember());
						if (proposal.getAllMembers() != null && !proposal.getAllMembers().isEmpty()) {
							for (MemberInfo mi : proposal.getAllMembers()) {
								IJavaElement je = mi.getJavaElement();
								if (je != null) {
									segment.addJavaElement(je);
								}
							}
						}
						if(segment.getUnpairedGettersOrSetters()!=null) {
							TypeInfoCollector.MethodInfo unpirMethod = segment.getUnpairedGettersOrSetters().get(filter);
							segment.clearUnpairedGettersOrSetters();
							if(unpirMethod!=null) {
								segment.getUnpairedGettersOrSetters().put(filter, unpirMethod);
							}
						}
						break;
					}
				} else if (presentationString.startsWith(filter)) {
					// JBIDE-512, JBIDE-2541 related changes ===>>>

					// This is used for CA.
					MemberInfo member = proposal.getMember();
					String sourceTypeName = member == null ? null : member.getDeclaringTypeQualifiedName();
					if (sourceTypeName != null && sourceTypeName.indexOf('.') != -1) 
						sourceTypeName = Signature.getSimpleName(sourceTypeName);
					String typeName = member == null ? null : member.getType().getName();
					if (typeName != null && typeName.indexOf('.') != -1) 
						typeName = Signature.getSimpleName(typeName);

					ELTextProposal kbProposal = new ELTextProposal();
					kbProposal.setReplacementString(proposal.getPresentation().substring(filter.length()));
					kbProposal.setLabel(proposal.getPresentationDisplayName());
					kbProposal.setImageDescriptor(getELProposalImageForMember(proposal.getMember()));
					kbProposal.setType(typeName);
					kbProposal.setSourceType(sourceTypeName);
					for (MemberInfo mi : proposal.getAllMembers()) {
						IJavaElement element = mi.getJavaElement();
						if (element != null) {
							kbProposal.addJavaElement(element);
						}
					}

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

				//Try properties if argument is a string.
				TypeInfoCollector infos = mbr.getTypeCollector(false, isStaticMethodsCollectingEnabled());
				if (TypeInfoCollector.isNotParameterizedCollection(mbr) || TypeInfoCollector.isResourceBundle(mbr.getMemberType())) {
					resolution.setMapOrCollectionOrBundleAmoungTheTokens(true);
				}
				proposalsToFilter.addAll(infos.getPropertyPresentations(segment.getUnpairedGettersOrSetters(), returnEqualedVariablesOnly));
				if(!proposalsToFilter.isEmpty()) {
					resolution.addSegment(segment);
					if(expr instanceof ELArgumentInvocation) {
						ELArgument a = ((ELArgumentInvocation)expr).getArgument();
						if(a.getArgument() == null) {
							segment.setToken(((ELObject)a).getFirstToken());
						} else {
							segment.setToken(a.getArgument().getFirstToken());
						}
					}
				}

				segment.setMemberInfo(mbr);
			}

			String filter = expr.getMemberName();
			boolean bSurroundWithQuotes = false;
			boolean closeQuotes = false;
			if(filter == null) {
				filter = ""; //$NON-NLS-1$
				bSurroundWithQuotes = true;
			} else {
				if((filter.startsWith("'") || filter.startsWith("\"")) //$NON-NLS-1$ //$NON-NLS-2$
					|| (filter.endsWith("'") || filter.endsWith("\""))) { //$NON-NLS-1$ //$NON-NLS-2$
					if (filter.startsWith("'") || filter.startsWith("\"")) { //$NON-NLS-1$ //$NON-NLS-2$
						filter = filter.length() == 1 ? "" : filter.substring(1); //$NON-NLS-1$	
						closeQuotes = true;
					}
					if (filter.endsWith("'") || filter.endsWith("\"")) { //$NON-NLS-1$ //$NON-NLS-2$
						filter = filter.length() == 1 ? "" : filter.substring(0, filter.length() - 1); //$NON-NLS-1$
						closeQuotes = false;
					}
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
						
						if (proposal.getAllMembers() != null && !proposal.getAllMembers().isEmpty()) {
							for (MemberInfo mi : proposal.getAllMembers()) {
								IJavaElement je = mi.getJavaElement();
								if (je != null) {
									segment.addJavaElement(je);
								}
							}
						}
						
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

					ELTextProposal kbProposal = new ELTextProposal();

					String replacementString = proposal.getPresentation().substring(filter.length());
					if (bSurroundWithQuotes) {
						replacementString = "'" + replacementString + "'"; //$NON-NLS-1$ //$NON-NLS-2$
					} else if(closeQuotes) {
						replacementString = replacementString + "'"; //$NON-NLS-1$
					}
					

					kbProposal.setReplacementString(replacementString);
					kbProposal.setLabel(proposal.getPresentationDisplayName());
					kbProposal.setImageDescriptor(getELProposalImageForMember(member));
					kbProposal.setType(typeName);
					kbProposal.setSourceType(sourceTypeName);
					for (MemberInfo mi : proposal.getAllMembers()) {
						IJavaElement element = mi.getJavaElement();
						if (element != null) {
							kbProposal.addJavaElement(element);
						}
					}

					kbProposals.add(kbProposal);
					// <<<=== JBIDE-512, JBIDE-2541 related changes
				}
			}
		}
		segment.setResolved(!resolution.getProposals().isEmpty());
		segment.setValidatable(!resolution.isMapOrCollectionOrBundleAmoungTheTokens());
	}

	protected boolean isSingularMember(TypeInfoCollector.MemberInfo mbr) {
		return false;
	}

	protected void processSingularMember(TypeInfoCollector.MemberInfo mbr, Set<TextProposal> kbProposals) {
	}

	protected void filterSingularMember(TypeInfoCollector.MemberInfo mbr, Set<TypeInfoCollector.MemberPresentation> proposalsToFilter) {
	}

	protected void setImage(TextProposal kbProposal, TypeInfoCollector.MemberPresentation proposal) {
		kbProposal.setImageDescriptor(getELProposalImage(proposal));
	}

	protected void setImage(TextProposal kbProposal, V var) {
		kbProposal.setImageDescriptor(getELProposalImageForMember(null));
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
	
	public static LexicalToken combineLexicalTokensForExpression(ELInvocationExpression expr) {
		// Create a combined lexical token to store all the variable name (not only the name before first dot, but all the name including all the words and dots)
		int variableTokenType = expr.getFirstToken().getType();
		int variableTokenStart = expr.getFirstToken().getStart();
		int variableTokenLength = 0;
		StringBuffer variableTokenText = new StringBuffer();
		LexicalToken current = expr.getFirstToken();
		LexicalToken variableTokenNext = null;
		while (current != null && current != expr.getLastToken()) {
			variableTokenText.append(current.getText());
			variableTokenLength += current.getLength();
			variableTokenNext = current.getNextToken();
			current = variableTokenNext;
		}
		if (current != null) {
			variableTokenText.append(current.getText());
			variableTokenLength += current.getLength();
			variableTokenNext = current.getNextToken();
		}				
		
		LexicalToken variableToken = new LexicalToken(variableTokenStart, variableTokenLength, variableTokenText, variableTokenType);
		variableToken.setNextToken(variableTokenNext);

		return variableToken;
	}
}