# ########################################################################################
# A CALCULUS DIFFERENTIATION OBJECT
# An implementation of algebraic differentiation by Jonathan Worthington.
# Copyright (C) Jonathan Worthington 2004
# This module may be used and distributed under the same terms as Perl.
# ########################################################################################

package Math::Calculus::Differentiate;
use strict;
our $VERSION = '0.2.1';

=head1 NAME

Math::Calculus::Differentiate - Algebraic Differentiation Engine

=head1 SYNOPSIS

  use Math::Calculus::Differentiate;

  # Create an object.
  my $exp = Math::Calculus::Differentiate->new;
  
  # Set a variable and expression.
  $exp->addVariable('x');
  $exp->setExpression('x^2 + 5*x') or die $exp->getError;
  
  # Differentiate and simplify.
  $exp->differentiate or die $exp->getError;;
  $exp->simplify or die $exp->getError;;
  
  # Print the result.
  print $exp->getExpression; # Prints 2*x + 5
  

=head1 DESCRIPTION

This module can take an algebraic expression, parse it into a tree structure, modify
the tree to give a representation of the differentiated function, simplify the tree
and turn the tree back into an output of the same form as the input.

It supports differentiation of expressions including the +, -, *, / and ^ (raise to
power) operators, bracketed expressions to enable correct precedence and the functions
ln, exp, sin, cos, tan, sec, cosec, cot, sinh, cosh, tanh, sech, cosech, coth, asin,
acos, atan, asinh, acosh and atanh.

=head1 EXPORT

None by default.

=head1 METHODS

=cut

# Constructor
# ###########

=item new

  $exp = Math::Calculus::Differentiate->new;

Creates a new instance of the differentiation engine, which can hold an individual
expression.

=cut

sub new {
	# Get invocant.
	my $invocant = shift;
	
	# Create object.
	my $self = {
		traceback	=> '',
		error		=> '',
		expression	=> 0,
		variables	=> [],
	};
	return bless $self, $invocant;
}


# Add variable.
# #############

=item addVariable

  $exp->addVariable('x');

Sets a certain named value in the expression as being a variable. A named value must be
an alphabetic chracter.

=cut

sub addVariable {
	# Get invocant and parameters.
	my ($self, $var) = @_;
	
	# Provided the variable is just one character and we don't already have it...
	unless (length($var) != 1 || grep { $_ eq $var } @{$self->{'variables'}}) {
		$self->{'variables'}->[@{$self->{'variables'}}] = $var;
		$self->{'error'} = '';
		return 1;
	} else {
		$self->{'error'} = 'Invalid variable or variable already added.';
		return undef;
	}
}


# Set Expression
# ##############

=item setExpression

  $exp->setExpression('x^2 + 5*x);

Takes an expression in human-readable form and stores it internally as a tree structure,
checking it is a valid expression that the module can understand in the process. Note that
the engine is strict about syntax. For example, note above that you must write 5*x and not
just 5x. Whitespace is allowed in the expression, but does not have any effect on precedence.
If you require control of precedence, use brackets; bracketed expressions will always be
evaluated first, as you would normally expect. The module follows the BODMAS precedence
convention. Returns undef on failure and a true value on success.

=cut

sub setExpression {
	# Get invocant and parameters.
	my ($self, $expr) = @_;
	
	# Clear up the expression.
	$expr =~ s/\s//g;
	1 while $expr =~ s/--/+/g
	     || $expr =~ s/\+-|-\+/-/g
	     || $expr =~ s/([+\-*\/\^])\+/$1/g
	     || $expr =~ s/^\+//g;
	
	# Build expression tree.
	$self->{'error'} = $self->{'traceback'} = undef;
	$self->{'expression'} = $self->buildTree($expr);
	
	# Return depending on whether there was an error.
	if ($self->{'error'}) {
		return undef;
	} else {
		return 1;
	}
}


# Get Expression
# ##############

=item getExpression

  $expr = $exp->getExpression;

Returns a textaul, human readable representation of the expression that is being stored.

=cut

sub getExpression {
	# Get invocant.
	my $self = shift;
	
	# Walk expression tree and generate something to display.
	$self->{'error'} = '';
	my $text = $self->prettyPrint($self->{'expression'});
	
	# If there was an error, return nothing.
	if ($self->{'error'}) {
		return undef;
	} else {
		return $text;
	}
}


# Differentiate.
# ##############

=item differentiate

  $exp->differentiate('x');

Differentiates the expression that was stored with setExpression with respect to the variable
passed as a parameter. Returns undef on failure and a true value on success.

=cut

sub differentiate {
	# Get invocant and variable.
	my ($self, $variable) = @_;
	
	# Check variable is in the list of variables.
	return undef unless grep { $_ eq $variable } @{$self->{'variables'}};
	
	# Clear error and traceback, and pass control to the differentiate routine.
	$self->{'error'} = $self->{'traceback'} = undef;
	eval {
		$self->{'expression'} = $self->differentiateTree($variable, $self->{'expression'});
	};
	
	# Return an appropriate value (or lack thereof...).
	if ($self->{'error'}) {
		return undef;
	} else {
		return 1;
	}
}


# Simplify.
# #########

=item simplify

  $exp->simplify;

Attempts to simplify the expression that is stored internally. It is a very good idea to call
this after calling differentiate, as the tree will often not be in the most compact possible
form, and this will affect the readability of output from getExpression and the performance
of future calls to differentiate if you are intending to obtain higher derivatives. Returns
undef on failure and a true value on success.

=cut

sub simplify {
	# Get invocant.
	my ($self) = @_;
	
	# Clear error.
	$self->{'error'} = undef;
	
	# Simplify.
	eval {
		$self->{'expression'} = $self->recSimplify($self->{'expression'}, undef);
	};
	
	# We may have boiled it all down to a numerical constant...
	my $const = $self->numericEvaluation($self->{'expression'});
	if (defined($const)) {
		$self->{'expression'} = $const;
	}
	
	# Return an appropriate value (or lack thereof...).
	if ($self->{'error'}) {
		return undef;
	} else {
		return 1;
	}
}


# Get traceback.
# ##############

=item getTraceback

  $exp->getTraceback;

When setExpression and differentiate are called, a traceback is generated to describe
what these functions did. If an error occurs, this traceback can be extremely useful
in helping track down the source of the error.

=cut

sub getTraceback {
	return $_[0]->{'traceback'};
}


# Get error.
# ##########

=item getError

  $exp->getError;

When any method other than getTraceback is called, the error message stored is cleared, and
then any errors that occur during the execution of the method are stored. If failure occurs,
call this method to get a textual representation of the error.

=cut

sub getError {
	return $_[0]->{'error'};
}


=head1 SEE ALSO

The author of this module has a website at L<http://www.jwcs.net/~jonathan/>, which has
the latest news about the module and a web-based frontend to allow you to test the module
out for yourself.

=head1 AUTHOR

Jonathan Worthington, E<lt>jonathan@jwcs.netE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2004 by Jonathan Worthington

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.1 or,
at your option, any later version of Perl 5 you may have available.

=cut


# ########################################################################################
# Private Methods
# ########################################################################################


# Build tree recursively explores the passed expression and generates a tree for it.
# The trees take a structure of an operation (which is +, -, *, /, ^, sin, cos, tan,
# sec, cosec, cot, sinh, cosh, tanh, sech, cosech, coth, asin, acos, atan, asinh,
# acosh, atanh, exp or ln) and two operands, which are either constants or references
# to other trees.
# ########################################################################################
sub buildTree {
	# Get invocant and expression.
	my ($self, $expr) = @_;
	
	# Store what we're parsing in the traceback.
	$self->{'traceback'} .= "Parsing $expr\n";
	
	# If it's a constant or single variable...
	if ($expr =~ /^ \(* (\-? ( (\d+(\.\d+)?) | [A-Za-z] )) \)* $/x) {
		# No tree to build; just return the expression.
		return $1;
	
	# Otherwise it could be a function.
	} elsif ($expr =~ /^ \(* (\-?) (a?sinh?|a?cosh?|a?tanh?|sech?|cosech?|coth?|ln|exp) \((.+)\) \)* $/x) {
		# Return single operand parse tree.
		return {
			operation	=> "$1$2",
			operand1	=> $self->buildTree($3),
			operand2	=> undef
		};
	} else {
		# Otherwise full analysis needed. Analyse expressiona and try to find a split point.
		my $error = undef;
		my $bestSplitOp = '';
		my $splitOpPos = 0;
		my $bracketDepth = 0;
		while (!$bestSplitOp && !$error) {
			# Cycle through all characters.
			my $curChar = 1;
			my $bracketDepthHitZero = 0;
			my $lastCharOp = 1;
			foreach my $char (split //, $expr) {
				# Maintain bracket depth.
				if ($char eq '(') {
					$bracketDepth ++;
				} elsif ($char eq ')') {
					$bracketDepth --;
				
				# Do we have a split point?
				} elsif ($curChar > 1 && $bracketDepth == 0 && $char =~ /[\^*\/+\-]/ &&
      	                     ($self->higherPrecedence($bestSplitOp, $char) || !$bestSplitOp)
				         && !$lastCharOp) {
					$splitOpPos = $curChar;
					$bestSplitOp = $char;
					
				}
				
				# If bracket depth is 0 and it's not the last character, record it happened.
				if ($bracketDepth == 0 && $curChar != length($expr)) {
					$bracketDepthHitZero = 1;
				}
				
				# If bracket depth is negative, we've got an error.
				if ($bracketDepth < 0)
				{
					$error = "Brackets not properly nested.";
				}
				
				# Maintain flag for if this character was an operator.
				$lastCharOp = $char =~ /[\^*\/+\-]/ ? 1 : 0;
				
				# Increment character counter.
				$curChar ++;
			}
			
			# If we have not found a split point, and the bracket depth hit 0, we have brackets
			# around all of the expression.
			if (!$error && !$bestSplitOp) {
				if (!$bracketDepthHitZero) {
					$expr =~ s/^\((.+)\)$/$1/;
				} else {
					$error = 'Could not split expression ' . $expr;
				}
			}
		}
		
		# If there wasn't an error, split, get operand and parse each subexpression.
		unless ($error) {
			my $operand1 = substr($expr, 0, $splitOpPos - 1);
			my $operand2 = substr($expr, $splitOpPos);
			if ($operand2 ne '') {
				return {
					operation	=> $bestSplitOp,
					operand1	=> $self->buildTree($operand1),
					operand2	=> $self->buildTree($operand2)
				};
			} else {
				$error = 'Could not split expression ' . $expr;
			}
		}
		
		# If we've got an error, store it and return failure.
		if ($error) {
			$self->{'error'} = $error;
			return undef;
		}
 	}
	
	# If we get here, something weird happened.
	$self->{'error'} = "Unknown error parsing $expr.";
	return undef;
}


# Pretty print takes an expression tree and returns a text representation for it.
# #######################################################################################
sub prettyPrint {
	# Get invocant and tree.
	my ($self, $tree, $lastOp) = @_;
	
	# See if the tree actually is a tree. If not, it's a value and just return it.
	unless (ref $tree) {
		return $tree;
	} else {
		# See how many operands we take.
		my $curOp = $tree->{'operation'};
		if ($curOp =~ /^[\^\/*\-+]$/) {
			# Dual operand. Look at last op to see if we need brackets.
			my $brackets = ($curOp eq '^' && $lastOp =~ /[\/*+\-]/ ||
			                $curOp =~ /[\/*]/ && $lastOp =~ /[*+\-]/ ||
			                $curOp =~ /[+\-]/ && $lastOp =~ /[+\-]/ ||
                                  !(defined($lastOp)) || $lastOp eq '(')
					   ? 0 : 1;
			
			# Pretty-print each operand, adding spaces around + and - ops.
			my $pretty = '';
			$pretty .= '(' if $brackets;
			$pretty .= $self->prettyPrint($tree->{'operand1'}, $curOp);
			$pretty .= ($curOp =~ /[+\-]/ ? ' ' : '') . $curOp . ($curOp =~ /[+\-]/ ? ' ' : '');
			$pretty .= $self->prettyPrint($tree->{'operand2'}, $curOp);
			$pretty .= ')' if $brackets;
			return $pretty;
		} else {
			# Single operand, e.g. function.
			return $curOp . '(' . $self->prettyPrint($tree->{'operand1'}, '(') . ')';
		}
	}
}


# Differentiate Tree explores the current expression tree, recursively differentiating
# the branches of the tree.
# ########################################################################################
sub differentiateTree {
	# Get invocant, variable and tree.
	my ($self, $variable, $tree) = @_;
	
	# Generate traceback.
	$self->{'traceback'} .= "Parsing " . $self->prettyPrint($tree) . "\n";
	
	# If we're at a node...
	unless (ref $tree) {
		# Is it the variable?
		if ($tree eq $variable) {
			# It goes to 1.
			return 1;
		
		# Or - the variable...
		} elsif ($tree eq "-$variable") {
			# It goes to -1.
			return -1;
		
		# Otherwise, it's a constant and goes to zero.
		} else {
			return 0;
		}
	} else {
		# We've got a complex expression. Our actions from here depend on what the
		# expression is.
		
		# Addition or subtraction - just differentiate each operand.
		if ($tree->{'operation'} eq '+' || $tree->{'operation'} eq '-') {
			return {
				operation	=> $tree->{'operation'},
				operand1	=> $self->differentiateTree($variable, $tree->{'operand1'}),
				operand2	=> $self->differentiateTree($variable, $tree->{'operand2'})
			};
		
		# Multiplication.
		} elsif ($tree->{'operation'} eq '*') {
			# Check if any branches are constant.
			my $o1c = $self->isConstant($variable, $tree->{'operand1'});
			my $o2c = $self->isConstant($variable, $tree->{'operand2'});
			
			# If they're both constant, return the tree as it is.
			if ($o1c && $o2c) {
				return $tree;
			
			# If the first is constant, only differentiate the second.
			} elsif ($o1c) {
				return {
					operation	=> $tree->{'operation'},
					operand1	=> $tree->{'operand1'},
					operand2	=> $self->differentiateTree($variable, $tree->{'operand2'})
				};
			
			# If the second is constant, only differentiate the first.
			} elsif ($o2c) {
				return {
					operation	=> $tree->{'operation'},
					operand1	=> $self->differentiateTree($variable, $tree->{'operand1'}),
					operand2	=> $tree->{'operand2'}
				};
			
			# Otherwise, it's the product rule. d[uv] = udv + vdu
			} else {
				return {
					operation	=> '+',
					operand1 	=>
						{
							operation	=> '*',
							operand1	=> $tree->{'operand1'},
							operand2	=> $self->differentiateTree($variable, $tree->{'operand2'})
						},
					operand2 	=>
						{
							operation	=> '*',
							operand1	=> $tree->{'operand2'},
							operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
						}
				};
			}
		
		# Division.
		} elsif ($tree->{'operation'} eq '/') {
			# Check if any branches are constant.
			my $o1c = $self->isConstant($variable, $tree->{'operand1'});
			my $o2c = $self->isConstant($variable, $tree->{'operand2'});
			
			# If they're both constant, return the tree as it is.
			if ($o1c && $o2c) {
				return $tree;
			
			# If the denominator is constant, just differentiate the top.
			} elsif ($o2c) {
				return {
					operation	=> '/',
					operand1	=> $self->differentiateTree($variable, $tree->{'operand1'}),
					operand2	=> $tree->{'operand2'}
				};
			
			# If the numerator is constant, e.g. k/u, then return k * d[u^-1].
			} elsif ($o1c) {
				my $uinv = {
					operation	=> '^',
					operand1	=> $tree->{'operand2'},
					operand2	=> -1
				};
				return {
					operation	=> '*',
					operand1	=> $tree->{'operand1'},
					operand2	=> $self->differentiateTree($variable, $uinv)
				}
			
			# Otherwise, neither is constant. Use d[u/v] = (vdu - udv) / v^2.
			} else {
				my $vdu = {
					operation	=> '*',
					operand2	=> $tree->{'operand2'},
					operand1	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
				my $udv = {
					operation	=> '*',
					operand2	=> $tree->{'operand1'},
					operand1	=> $self->differentiateTree($variable, $tree->{'operand2'})
				};
				return {
					operation	=> '/',
					operand1	=>
						{
							operation	=> '-',
							operand1	=> $vdu,
							operand2	=> $udv
						},
					operand2	=>
						{
							operation	=> '^',
							operand1	=> $tree->{'operand2'},
							operand2	=> 2
						}
				};
			}	
			
			
		# Powers.
		} elsif ($tree->{'operation'} eq '^') {
			# Check if any branches are constant.
			my $o1c = $self->isConstant($variable, $tree->{'operand1'});
			my $o2c = $self->isConstant($variable, $tree->{'operand2'});
			
			# If they're both constant, return the tree as it is.
			if ($o1c && $o2c) {
				return $tree;
			
			# If the power is constant...
			} elsif ($o2c) {
				# d[(f(x))^n] = n*f'(x)*f(x)^(n-1)
				return {
					operation	=> '*',
					operand1	=> $tree->{'operand2'},
					operand2	=>
						{
							operation	=> '*',
							operand1	=> $self->differentiateTree($variable, $tree->{'operand1'}),
							operand2	=>
								{
									operation	=> '^',
									operand1	=> $tree->{'operand1'},
									operand2	=>
										{
											operation	=> '-',
											operand1	=> $tree->{'operand2'},
											operand2	=> 1
										}
								}
						}
				};
			
			# If the value being raised to a power is constant...
			} elsif ($o1c) {
				# d[k^v] = dv * ln(k) * exp(ln(k) * v)
				my $dv = $self->differentiateTree($variable, $tree->{'operand2'});
				my $lnk = {
					operation	=> 'ln',
					operand1	=> $tree->{'operand1'},
					operand2	=> undef
				};
				return {
					operation	=> '*',
					operand1	=> $dv,
					operand2	=>
						{
							operation	=> '*',
							operand1	=> $lnk,
							operand2	=>
								{
									operation	=> 'exp',
									operand1	=>
										{
											operation	=> '*',
											operand1	=> $lnk,
											operand2	=> $tree->{'operand2'}
										},
									operand2	=> undef
								}
						}
				};
				
			
			# If it's a function of the variable raised to another function of the variable...
			} else {
				# d[u^v] = exp(ln(u) * v) * ((vdu)/u + ln(u)dv)
				my $lnu = {
					operation	=> 'ln',
					operand1	=> $tree->{'operand1'},
					operand2	=> undef
				};
				my $dv = $self->differentiateTree($variable, $tree->{'operand2'});
				my $vdu = {
					operation	=> '*',
					operand1	=> $tree->{'operand2'},
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
				return {
					operation	=> '*',
					operand1	=>
						{
							operation	=> 'exp',
							operand1	=>
								{
									operation	=> '*',
									operand1	=> $lnu,
									operand2	=> $tree->{'operand2'}
								},
							operand2	=> undef
						},
					operand2	=>
						{
							operation	=> '+',
							operand1	=>
								{
									operation	=> '/',
									operand1	=> $vdu,
									operand2	=> $tree->{'operand1'}
								},
							operand2	=>
								{
									operation	=> '*',
									operand1	=> $lnu,
									operand2	=> $dv
								}
						}
				};
			}
		
		# Natural logarithm
		} elsif ($tree->{'operation'} =~ /^(\-?)ln$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[ln(u)] = du/u
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> "${neg}1",
				operand2	=>
					{
						operation	=> '/',
						operand1	=> $du,
						operand2	=> $tree->{'operand1'}
					}
			};
			
		# Exponential (e)
		} elsif ($tree->{'operation'} =~ /^(\-?)exp$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[exp(u)] = exp(u)du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> $du,
				operand2	=> $tree
			};
			
		# sin
		} elsif ($tree->{'operation'} =~ /^(\-?)sin$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[sin(u)] = cos(u)du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> "${neg}cos",
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			};
		
		# cos
		} elsif ($tree->{'operation'} =~ /^(\-?)cos$/) {
			# Stash negativity.
			my $neg = $1 eq '-' ? '' : '-';
			
			# d[cos(u)] = -sin(u)du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> "${neg}sin",
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			};
		
		# tan
		} elsif ($tree->{'operation'} =~ /^(\-?)tan$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[tan(u)] = (sec(u))^2 * du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> "${neg}1",
				operand2	=>
					{
						operation	=> '*',
						operand1	=> $du,
						operand2	=>
							{
								operation	=> '^',
								operand1	=>
									{
										operation	=> "sec",
										operand1	=> $tree->{'operand1'},
										operand2	=> undef
									},
								operand2	=> 2
							}
					}
			};
		
		# sec
		} elsif ($tree->{'operation'} =~ /^(\-?)sec$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/cos and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'cos',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# cosec
		} elsif ($tree->{'operation'} =~ /^(\-?)cosec$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/sin and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'sin',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# cot
		} elsif ($tree->{'operation'} =~ /^(\-?)cot$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/tan and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'tan',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# sinh
		} elsif ($tree->{'operation'} =~ /^(\-?)sinh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[sinh(u)] = cosh(u)du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> "${neg}cosh",
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			};
		
		# cosh
		} elsif ($tree->{'operation'} =~ /^(\-?)cosh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[cosh(u)] = sinh(u)du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> "${neg}sinh",
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			};
		
		# tanh
		} elsif ($tree->{'operation'} =~ /^(\-?)tanh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[tanh(u)] = (sech(u))^2 * du
			my $du = $self->differentiateTree($variable, $tree->{'operand1'});
			return {
				operation	=> '*',
				operand1	=> "${neg}1",
				operand2	=>
					{
						operation	=> '*',
						operand1	=> $du,
						operand2	=>
							{
								operation	=> '^',
								operand1	=>
									{
										operation	=> "sech",
										operand1	=> $tree->{'operand1'},
										operand2	=> undef
									},
								operand2	=> 2
							}
					}
			};
		
		# sech
		} elsif ($tree->{'operation'} =~ /^(\-?)sech$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/cosh and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'cosh',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# cosech
		} elsif ($tree->{'operation'} =~ /^(\-?)cosech$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/sinh and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'sinh',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# coth
		} elsif ($tree->{'operation'} =~ /^(\-?)coth$/) {
			# Stash negativity.
			my $neg = $1;
			
			# Convert to 1/tanh and differentiate.
			return $self->differentiateTree($variable, {
				operation	=> '/',
				operand1	=> "${neg}1",
				operand2	=> 
					{
						operation	=> 'tanh',
						operand1	=> $tree->{'operand1'},
						operand2	=> undef
					}
			});
		
		# asin
		} elsif ($tree->{'operation'} =~ /^(\-?)asin$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[asin(u)] = du / (1 - u^2)^0.5
			my $du;
			if ($neg) {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			} else {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '^',
						operand1	=>
							{
								operation	=> '-',
								operand1	=> 1,
								operand2	=>
									{
										operation	=> '^',
										operand1	=> $tree->{'operand1'},
										operand2	=> 2
									}
							},
						operand2	=> 0.5
					}
			};
		
		# acos
		} elsif ($tree->{'operation'} =~ /^(\-?)acos$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[acos(u)] = -du / (1 - u^2)^0.5
			my $du;
			if ($neg) {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			} else {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '^',
						operand1	=>
							{
								operation	=> '-',
								operand1	=> 1,
								operand2	=>
									{
										operation	=> '^',
										operand1	=> $tree->{'operand1'},
										operand2	=> 2
									}
							},
						operand2	=> 0.5
					}
			};
		
		# atan
		} elsif ($tree->{'operation'} =~ /^(\-?)atan$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[atan(u)] = du / (1 + u^2)
			my $du;
			if ($neg) {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			} else {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '+',
						operand1	=> 1,
						operand2	=>
							{
								operation	=> '^',
								operand1	=> $tree->{'operand1'},
								operand2	=> 2
							}
					}
			};
		
		# asinh
		} elsif ($tree->{'operation'} =~ /^(\-?)asinh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[asinh(u)] = du / (1 + u^2)^0.5
			my $du;
			if ($neg) {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			} else {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '^',
						operand1	=>
							{
								operation	=> '+',
								operand1	=> 1,
								operand2	=>
									{
										operation	=> '^',
										operand1	=> $tree->{'operand1'},
										operand2	=> 2
									}
							},
						operand2	=> 0.5
					}
			};
		
		# acosh
		} elsif ($tree->{'operation'} =~ /^(\-?)acosh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[acosh(u)] = du / (u^2 - 1)^0.5
			my $du;
			if ($neg) {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			} else {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '^',
						operand1	=>
							{
								operation	=> '-',
								operand1	=>
									{
										operation	=> '^',
										operand1	=> $tree->{'operand1'},
										operand2	=> 2
									},
								operand2	=> 1
							},
						operand2	=> 0.5
					}
			};
		
		# atanh
		} elsif ($tree->{'operation'} =~ /^(\-?)atanh$/) {
			# Stash negativity.
			my $neg = $1;
			
			# d[atanh(u)] = du / (1 - u^2)
			my $du;
			if ($neg) {
				$du = {
					operation	=> '-',
					operand1	=> '0',
					operand2	=> $self->differentiateTree($variable, $tree->{'operand1'})
				};
			} else {
				$du = $self->differentiateTree($variable, $tree->{'operand1'});
			}
			return {
				operation	=> '/',
				operand1	=> $du,
				operand2	=>
					{
						operation	=> '-',
						operand1	=> 1,
						operand2	=>
							{
								operation	=> '^',
								operand1	=> $tree->{'operand1'},
								operand2	=> 2
							}
					}
			};
		
		# Otherwise, we don't know what it is.
		} else {
			$self->{'error'} = "Could not differentiate " . $self->prettyPrint($tree);
			die;
		}
	}
}


# recSimplify recursively walks a tree and simplifies the branches, then the current
# node.
# ########################################################################################
sub recSimplify {
	# Get invocant, variable and tree.
	my ($self, $tree) = @_;
	
	# If it's just a node, return it. We can't do a great deal with nodes.
	return $tree unless ref $tree;
	
	# Otherwise, simplify each operand before we go any further.
	my $left = $self->recSimplify($tree->{'operand1'});
	my $right = $self->recSimplify($tree->{'operand2'});

	## CONSTANT EVALUATION
	
	# Get any available numeric evaluations of the left and right branches.
	my $leftval = $self->numericEvaluation($left);
	my $rightval = $self->numericEvaluation($right);
	
	# If they have a numeric evaluation, assign them to the actual values.
	$left = $leftval if defined($leftval);
	$right = $rightval if defined($rightval);
	
	## NULL OPERATORS
	
	# 0 + x = x + 0 = x
	if ($tree->{'operation'} eq '+' && (!(ref $left) && $left eq '0')) {
		return $right;
	}
	if ($tree->{'operation'} eq '+' && (!(ref $right) && $right eq '0')) {
		return $left;
	}
	
	# x - 0 = x
	if ($tree->{'operation'} eq '-' && (!(ref $right) && $right eq '0')) {
		return $left;
	}
	
	# 1 * x = x * 1 = x
	if ($tree->{'operation'} eq '*' && (!(ref $left) && $left eq '1')) {
		return $right;
	}
	if ($tree->{'operation'} eq '*' && (!(ref $right) && $right eq '1')) {
		return $left;
	}
	
	# x / 1 = x
	if ($tree->{'operation'} eq '/' && (!(ref $right) && $right eq '1')) {
		return $left;
	}
	
	# x ^ 1 = x
	if ($tree->{'operation'} eq '^' && (!(ref $right) && $right eq '1')) {
		return $left;
	}
	
	## EFFECTS OF ZERO
	
	# x ^ 0 = 1
	if ($tree->{'operation'} eq '^' && (!(ref $right) && $right eq '0')) {
		return 1;
	}
	
	# 0 * x = x * 0 = 0
	if ($tree->{'operation'} eq '*' && (!(ref $left) && $left eq '0')) {
		return 0;
	}
	if ($tree->{'operation'} eq '*' && (!(ref $right) && $right eq '0')) {
		return 0;
	}
	
	# 0 / x = 0
	if ($tree->{'operation'} eq '/' && (!(ref $left) && $left eq '0')) {
		return 0;
	}
	
	## DIVISION OF AN EXPRESSION BY ITSELF
	
	# x / x = 1
	if ($tree->{'operation'} eq '/' && $self->isIdentical($left, $right)) {
		return 1;
	}
	
	## SUBTRACTION OF AN EXPRESSION FROM ITSELF
	
	# x / x = 1
	if ($tree->{'operation'} eq '-' && $self->isIdentical($left, $right)) {
		return 0;
	}
	
	## DEEP NUMERICAL CONSTANT COMBINATION
	
	# n * (m * x) = (o * x) where o = nm
	if ($tree->{'operation'} eq '*' && ref($right) && $right->{'operation'} eq '*' &&
	    !(ref($left)) && $left =~ /^-?\d+(\.\d+)?$/ && $right->{'operand1'} =~ /^-?\d+(\.\d+)?$/) {
		return {
			operation	=> '*',
			operand1	=> ($left * $right->{'operand1'}),
			operand2	=> $right->{'operand2'}
		};
	
	# n * (x * m) = (o * x) where o = nm
	} elsif ($tree->{'operation'} eq '*' && ref($right) && $right->{'operation'} eq '*' &&
	    !(ref($left)) && $left =~ /^-?\d+(\.\d+)?$/ && $right->{'operand2'} =~ /^-?\d+(\.\d+)?$/) {
		return {
			operation	=> '*',
			operand1	=> ($left * $right->{'operand2'}),
			operand2	=> $right->{'operand1'}
		};
	
	# (m * x) * n = (o * x) where o = nm
	} elsif ($tree->{'operation'} eq '*' && ref($left) && $left->{'operation'} eq '*' &&
	    !(ref($right)) && $right =~ /^-?\d+(\.\d+)?$/ && $left->{'operand1'} =~ /^-?\d+(\.\d+)?$/) {
		return {
			operation	=> '*',
			operand1	=> ($right * $left->{'operand1'}),
			operand2	=> $right->{'operand2'}
		};
	
	# (x * m) * n = (o * x) where o = nm
	} elsif ($tree->{'operation'} eq '*' && ref($left) && $left->{'operation'} eq '*' &&
	    !(ref($right)) && $right =~ /^-?\d+(\.\d+)?$/ && $left->{'operand2'} =~ /^-?\d+(\.\d+)?$/) {
		return {
			operation	=> '*',
			operand1	=> ($right * $left->{'operand2'}),
			operand2	=> $right->{'operand1'}
		};
	}
	
	## NATURAL LOGARITHM AND EXPONENTIATION INVERSTION
	
	# exp(ln(f(x))) = f(x)
	if ($tree->{'operation'} =~ /^-?exp$/ && ref($left) && $left->{'operation'} =~ /^ln$/) {
		if ($tree->{'operation'} =~ /^-/) {
			return {
				operation	=> '*',
				operand1	=> "-1",
				operand2	=> $left->{'operand1'}
			};
		} else {
			return $left->{'operand1'};
		}
	}
	
	# ln(exp(f(x))) = f(x)
	if ($tree->{'operation'} =~ /^-?ln$/ && ref($left) && $left->{'operation'} =~ /^exp$/) {
		if ($tree->{'operation'} =~ /^-/) {
			return {
				operation	=> '*',
				operand1	=> "-1",
				operand2	=> $left->{'operand1'}
			};
		} else {
			return $left->{'operand1'};
		}
	}
		
	## NO SIMPLIFICATION POSSIBLE - BUILD NEW TREE OF SIMPLIFIED SUBTREES
	
	# If we get here, just build and return a new tree, which may have no changes.
	return {
		operation	=> $tree->{'operation'},
		operand1	=> $left,
		operand2	=> $right
	};
}


# higherPrecedence(a, b) returns true if a has higher or equal precedence than b.
# ########################################################################################
sub higherPrecedence {
	# Get invocant and parameters.
	my ($self, $a, $b) = @_;
	
	# Do precedence check.
	if ($a eq '^') {
		return 1;
	} elsif ($a eq '/' && $b =~ /\/|\*|\+|-/) {
		return 1;
	} elsif ($a eq '*' && $b =~ /\*|\+|-/) {
		return 1;
	} elsif ($a eq '+' && $b =~ /\+|-/) {
		return 1;
	} elsif ($a eq '-' && $b eq '-') {
		return 1;
	}
		
	# If we get here, precedence is lower.
	return 0;
}


# isConstant takes a tree and a variable, checks if it's dependent on that variable and
# returns 1 if so and 0 if not.
# ########################################################################################
sub isConstant {
	# Get invocant, variable and tree.
	my ($self, $variable, $tree) = @_;
	
	# If the tree is undefined, we've run off the end of it, which means it was all constant.
	return 1 unless defined($tree);
	
	# If we have a ref...
	if (ref $tree) {
		return ($self->isConstant($variable, $tree->{'operand1'}) && $self->isConstant($variable, $tree->{'operand2'}));
	} else {
		# Atom. But is it the variable?
		return $tree eq $variable || $tree eq "-$variable" ? 0 : 1;
	}
}


# Numeric Evaluation takes a tree and, provided it is constant and all constants are
# numeric, calculates the value of the tree. Returns undef if numeric evaluation is
# not possible.
# ########################################################################################
sub numericEvaluation {
	# Get invocant and tree.
	my ($self, $tree) = @_;
	
	# If the tree is a value...
	unless (ref $tree) {
		# If it's numeric, return it.
		return $tree =~ /^-?\d+(\.\d+)?$/ ? $tree : undef;
	} else {
		# Attempt to numerically evaluate each branch.
		my $leftval = $self->numericEvaluation($tree->{'operand1'});
		my $rightval = $self->numericEvaluation($tree->{'operand2'});
		
		# If it's an addition op and both values are numeric...
		if ($tree->{'operation'} eq '+' && defined($leftval) && defined($rightval)) {
			# Add and return.
			return $leftval + $rightval;
		
		# If it's a subtraction op and both values are numeric...
		} elsif ($tree->{'operation'} eq '-' && defined($leftval) && defined($rightval)) {
			# Subtract and return.
			return $leftval - $rightval;

		# If it's a multiplication op and both values are numeric...
		} elsif ($tree->{'operation'} eq '*' && defined($leftval) && defined($rightval)) {
			# Multiply and return.
			return $leftval * $rightval;
		
		# If it's a power op and both values are numeric...
		} elsif ($tree->{'operation'} eq '^' && defined($leftval) && defined($rightval)) {
			# Multiply and return.
			return $leftval ^ $rightval;
		 		# Otherwise, we can't do numerical operations. Return undef.
		} else {
			return undef;
		}
	}
}


# isIdentical takes two trees and checks if they are identical. Note that identical might
# not mean equal.
# ########################################################################################
sub isIdentical {
	# Get invocant and trees.
	my ($self, $treeA, $treeB) = @_;
	
	# If both are not references and they are the same...
	if (!ref($treeA) && !ref($treeB) && $treeA eq $treeB) {
		return 1;
	
	# If they are both references and have the same operators...
	} elsif (ref($treeA) && ref($treeB) && $treeA->{'operation'} eq $treeB->{'operation'}) {
		# Recursively compare the subtrees.
		my $leftcomp = $self->isIdentical($treeA->{'operand1'}, $treeB->{'operand1'});
		my $rightcomp = $self->isIdentical($treeA->{'operand2'}, $treeB->{'operand2'});
		return $leftcomp && $rightcomp ? 1 : 0;
	
	# Otherwise, they must not be the same.
	} else {
		return 0;
	}
}


1;





