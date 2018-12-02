using System;
using System.Linq;

namespace Demo
{
    /// <summary>
    /// A superhero.
    /// </summary>
    /// <refinement>
    /// Arbitrary<Superhero> Is() 
    /// {
    ///     var attributeGen = from a in Arb.Generate<string>().Two()
    ///                        select a.Item1 + " " + a.Item2;
    ///     var nameGen = Arb.Generate<string>().Where(s => s != null);
    ///     return Arb.From(from a in attributeGen.NonEmptyListOf()
    ///                     from n in nameGen
    ///                     select new Superhero(n, a.ToArray()));
    /// }
    /// </refinement>
    public class Superhero : IEquatable<Superhero>
    {
        /// <summary>
        /// Returns the Superhero's abilities.  Each ability must be two words long.
        /// </summary>
        /// <remarks>ex: "sun gaze", "dangerous flute", etc.</remarks>
        public string[] Abilities { get; }
        public string Name { get; }

        /// <spec>bool NameNotNull(string s) => s == null ? Throws<ArgumentException>(() => new Superhero(s)) : new Superhero(s).Name == s;</spec>
        public Superhero(string name, params string[] abilities)
        {
            if (name == null)
                throw new ArgumentException("name can't be null!");

            Name = name;
            Abilities = abilities;
        }

        /// <summary>
        /// Computes the power level of the superhero.  Always a positive value.
        /// </summary> 
        /// <spec>bool Positive(NonNull<string> s) => new Superhero(s.Get).GetPowerLevel() >= 0;</spec>
        public int GetPowerLevel()
        {
            return Math.Abs(Name.GetHashCode());
        }

        /// <spec>bool PropertywiseEquals(Superhero hero) => hero.Equals(hero);</spec>
        /// <spec>bool NullThrows(Superhero hero) => Throws<NullReferenceException>(() => hero.Equals(null));</spec>
        public bool Equals(Superhero other)
        {
            // There is a bug in this function, and the specs, above, hint at it.
            // Can you find the bug?

            return this.Name == other.Name
                && this.Abilities.SequenceEqual(other.Abilities);
        }

        public override string ToString() => Name;
    }
}
