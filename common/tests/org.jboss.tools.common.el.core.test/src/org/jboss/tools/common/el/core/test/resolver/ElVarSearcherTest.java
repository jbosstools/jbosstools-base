package org.jboss.tools.common.el.core.test.resolver;

import java.io.IOException;
import java.util.Collections;
import java.util.List;

import junit.framework.TestCase;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.text.BadLocationException;
import org.jboss.tools.common.el.core.model.ELExpression;
import org.jboss.tools.common.el.core.parser.ELParserFactory;
import org.jboss.tools.common.el.core.parser.ELParserUtil;
import org.jboss.tools.common.el.core.resolver.ELCompletionEngine;
import org.jboss.tools.common.el.core.resolver.ELContext;
import org.jboss.tools.common.el.core.resolver.ELResolution;
import org.jboss.tools.common.el.core.resolver.ELResolutionImpl;
import org.jboss.tools.common.el.core.resolver.ElVarSearcher;
import org.jboss.tools.common.el.core.resolver.IRelevanceCheck;
import org.jboss.tools.common.el.core.resolver.Var;
import org.jboss.tools.common.text.TextProposal;
import org.jboss.tools.test.resource.ResourceFactory;

public class ElVarSearcherTest extends TestCase{

	public void testSetFile() {
		ELCompletionEngine fakeEngine = new FakeELCompletionEngine();
		IFile file = ResourceFactory.createFile("${test}");
		ElVarSearcher varSearcher = new ElVarSearcher(null,fakeEngine);
		varSearcher.setFile(file);
	}

	public void testFindAllVarsIFileInt() throws CoreException, IOException {
		ELCompletionEngine fakeEngine = new FakeELCompletionEngine();
		String fileContent = "<html><body var=\"test1\" value=\"#{value1}\"><p var=\"test2\" value=\"#{value2}\"/></body></html>";
		IFile file = ResourceFactory.createFile(fileContent,"Test1","test.xml");
		ElVarSearcher varSearcher = new ElVarSearcher(file,fakeEngine);
		List<Var> vars = varSearcher.findAllVars(file,fileContent.indexOf("#{value2}"));
		assertTrue(vars.size()==2);
	}

	private static class FakeELCompletionEngine implements ELCompletionEngine {
		public ELResolution resolveELOperand(IFile file,
				ELExpression operand, boolean returnEqualedVariablesOnly,
				List<Var> vars, ElVarSearcher varSearcher, int offset)
				throws BadLocationException, StringIndexOutOfBoundsException {
			return new ELResolutionImpl(operand);
		}

		public ELParserFactory getParserFactory() {
			return ELParserUtil.getJbossFactory();
		}

		public List<TextProposal> getProposals(ELContext context, String el, int offset) {
			return Collections.emptyList();
		}

		public ELResolution resolve(ELContext context, ELExpression operand, int offset) {
			return new ELResolutionImpl(operand);
		}

		public List<TextProposal> getProposals(ELContext context, int offset) {
			return Collections.emptyList();
		}

		public IRelevanceCheck createRelevanceCheck(IJavaElement element) {
			return null;
		}
	}
}
